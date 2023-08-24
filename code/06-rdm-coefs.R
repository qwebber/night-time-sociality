

### Packages ----
libs <- c('data.table', 'ggplot2', 'gridExtra',
					'lme4', 'emmeans')
lapply(libs, require, character.only = TRUE)

### Input data ----
df <- readRDS('output/rdmNets-1000.RDS')
df$logStrength <- log(df$strength + 0.125)

dfWin <- df[season == "winter"]
dfSum <- df[season == "summer"]

i = 1
coefs2 <- c()
ems2 <- c()

for(i in 1:1000){

mod <- lmer(logStrength ~  tod * Cover + as.factor(yr) +
							(1|ID),
						data = dfWin[iteration == i])

em <- emmeans(mod, ~ tod * Cover)

## pairwise comparison corrected using Tukey's test
aa <- data.frame(pairs(em, adjust="tukey"))

ems <- data.table(contrast = aa$contrast,
									estimate = aa$estimate,
									iter = i)

coefs <- data.table(night = mean(coef(mod)$ID[2]$todnight),
										lichen = mean(coef(mod)$ID[3]$CoverLichen),
										open = mean(coef(mod)$ID[4]$CoveropenMove),
										yr18 = mean(coef(mod)$ID[5]$`as.factor(yr)2018`),
										yr19 = mean(coef(mod)$ID[6]$`as.factor(yr)2019`),
										nightLichen = mean(coef(mod)$ID[7]$`todnight:CoverLichen`),
										nightOpen = mean(coef(mod)$ID[8]$`todnight:CoveropenMove`),
										iter = i)

ems2[[i]] <- ems
coefs2[[i]] <- coefs

}

ems3 <- rbindlist(ems2)
coefs3 <- rbindlist(coefs2)

## export files
fwrite(ems3, 'output/rdm-ems.csv')
fwrite(coefs3, 'output/rdm-coefs.csv')
