
## load packages
library(data.table)
library(ggplot2)
library(lme4)
library(performance)
library(emmeans)
library(Rmisc)
library(dplyr) # added for data manipulation
## missing some packages (I didn't have lmerTest and pbkrtest installed)

## load data
fogo <- fread("output/tod-networks.csv")
coefs <- fread("output/rdm-coefs.csv")
ems <- fread("output/rdm-ems.csv") %>%
	mutate(contrast = gsub("day", "Day", #cleaning contrast labels for consistency
												 gsub("night", "Night",
												 		 gsub("openMove", "Rocky barrens", contrast)))) # I made a leap here that openMove is rocky barrens?

fogo[, .N, by = c("yr", "tod", "season", "Cover")]

## your working directory may be something like this...
## I just used desktop, but the file path will be whereever you save the file
#fogo <- fread("/Users/maryatkinson/Desktop/tod-networks.csv")

## look at the top 6 rows of the data file
head(fogo)

## how many unique individuals are there?
unique(fogo$ID)

## how many data points in each season?
fogo[, .N, by = "season"]

## how many data points in each tod?
fogo[, .N, by = "tod"]

## check distribution of strength (we know from past work it is usually not normal)
hist(fogo$strength)

## log-transform strength because it has a non-normal distribution
fogo$logStrength <- log(fogo$strength + 0.125)

## change names of variables
fogo$Cover[fogo$Cover == "openMove"] <- "Rocky barrens"
fogo$season[fogo$season == "spring"] <- "Spring"
fogo$season[fogo$season == "winter"] <- "Winter"
fogo$tod[fogo$tod == "day"] <- "Day"
fogo$tod[fogo$tod == "night"] <- "Night"

## regular linear regression
model1 <- lm(logStrength ~ tod + season + Cover, data = fogo)
summary(model1)

## simple mixed model
model2 <- lmer(logStrength ~ tod + season + Cover + (1|ID), data = fogo)
summary(model2)

## more complicated mixed model
model3win <- lmer(logStrength ~  tod * Cover + as.factor(yr) +
							 	(1|ID),
							 data = fogo[season == "Winter"])
jtools::summ(model3win)

model3sum <- lmer(logStrength ~  tod * Cover + as.factor(yr) +
										(1|ID),
									data = fogo[season == "Spring"])
jtools::summ(model3sum)

fogo$IDYr <- as.factor(paste(fogo$ID, fogo$yr, sep = "_"))

## model checks
model_performance(model3win)
model_performance(model3sum)

## emmeans provides the predicted value of strength for all combinations of tod, cover, and season
emdat <- data.frame(emmeans(model3win, ~ tod * Cover))

setnames(emdat, "tod", "Time of Day")

## leave out season from the Tukey's test comparison
emW <- emmeans(model3win, ~ tod * Cover)
emS <- emmeans(model3sum, ~ tod * Cover)

## pairwise comparison corrected using Tukey's test
pairsW <- pairs(emW, adjust = "tukey")
pairsS <- pairs(emS, adjust = "tukey")

## plot results
png("figures/fig2.png",
		width = 3500,
		height = 2500,
		res = 600,
		units = "px")
ggplot(data=emdat, aes(x=Cover,y=emmean, fill=`Time of Day`)) + ## FYI this is not reproducible, gives an error message missing season variable
	geom_point(aes(color = `Time of Day`),
						 stat="identity",
						 position = position_dodge(width = 0.5)) +
	scale_color_manual(values = c("#e34a33", "#2b8cbe")) +
	geom_errorbar(aes(ymin = (emmean - SE),
										ymax = (emmean + SE),
										color = `Time of Day`),
						 position = position_dodge(width = 0.5),
						 width = 0) +
	xlab("Habitat type") +
	ylab("Estimated marginal mean (graph strength)") +
	theme(legend.position = "right",
				legend.key = element_blank(),
				strip.background = element_rect(color = "black",
																				fill = "white",
																				size = 1),
				strip.text = element_text(size = 10,
																	color = "black"),
				axis.text = element_text(size = 10,
																 color = "black"),
				axis.title = element_text(size = 14),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(color = "black",
																		fill=NA, size = 1)) +
	facet_wrap(~season)
dev.off()

#### FIGURE 2 #####

png("figures/fig2.png",
		width = 4000,
		height = 4000,
		res = 600,
		units = "px")
ggplot(fogo, aes(Cover, logStrength, color = IDYr, group = IDYr)) +
	geom_point() +
	geom_line() +
	scale_color_viridis_d() +
	xlab("Habitat type") +
	ylab("log(social strength)") +
	theme(legend.position = 'none',
				strip.background = element_rect(color = "black",
																				fill = "white",
																				size = 1),
				strip.text = element_text(size = 10,
																	color = "black"),
				axis.text = element_text(size = 10,
																 color = "black"),
				axis.title = element_text(size = 14),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(color = "black", fill=NA, size = 1)) +
	facet_wrap(~season*tod)
dev.off()



#### FIGURE 3 #####
pairsW <- as.data.table(pairsW) # translate model output to estimate data

#calculate the quantiles in advance, join with the estimates from contrasts for plotting
ems_quantiles <- ems %>%
	group_by(contrast) %>%
	summarize(lower = quantile(estimate, probs = .025),
						upper = quantile(estimate, probs = .975),
						median = median(estimate),
						mid_lower = quantile(estimate, probs = .25),
						mid_upper = quantile(estimate, probs = .75),
						min = min(estimate),
						max = max(estimate)) %>%
	mutate(contrast = gsub("day", "Day",
												 gsub("night", "Night",
												 		 gsub("openMove", "Rocky barrens", contrast)))) %>%
	left_join(., pairsW) %>%
	mutate(type = ifelse((estimate<lower|estimate>upper), "Significant", "Non-Significant"),
				 contrast_N = 1:15)

ems <-left_join(ems, ems_quantiles[,c("contrast","type")], by = "contrast") # if you want to colour by significance

theme_pairs <- theme(legend.position = 'none',
											strip.background = element_rect(color = "black",
																											fill = "white",
																											size = 1),
											strip.text = element_text(size = 10,
																								color = "black"),
											axis.text = element_text(size = 20,
																							 color = "black"),
											axis.title = element_text(size = 26),
										  plot.title = element_text(size = 10),
											panel.grid.minor = element_blank(),
											panel.background = element_blank(),
											panel.border = element_rect(color = "black", fill=NA, size = 1))


#significance indicated by coloured estimate, determined by whether the estimate is outside of the 2.5-97.5 quantiles
png("figures/fig3.png",
		width = 10000,
		height = 6000,
		res = 600,
		units = "px")

ggplot(data = ems_quantiles) +
	geom_linerange(aes(x = contrast_N, ymin = min, ymax = max)) + # shows the extent of the entire range, can be removed if we want to stick with the .025/.975
	geom_rect(aes(xmin = contrast_N-.2, xmax = contrast_N+.2, ymin = lower, ymax = upper), fill = "grey50") +
	geom_rect(aes(xmin = contrast_N-.4, xmax = contrast_N+.4, ymin = mid_lower, ymax = mid_upper), fill = "grey25")+
	#geom_point(data = ems_quantiles, aes(x = contrast_N, y = median), color = "black", size = 2, shape = 20) + #displays median point if we want to display
	geom_point(aes(x = contrast_N, y = estimate), size = 4.5, color = "black") +
	geom_point(aes(x = contrast_N, y = estimate, color = type), size = 4) +
	scale_color_manual(values = c("white","red")) + #red indicates significance by p-value
	coord_flip() +
	labs(y = "Estimate", x = NULL) +
	scale_x_continuous( breaks = 1:15, labels = ems_quantiles$contrast) +
	theme_pairs
dev.off()

#this is the original figure style in simpler code (easier to read)
#does not have A) to O) labels currently
#ggplot(data = ems, aes(x = estimate)) + geom_histogram(binwidth = 0.025) + facet_wrap(~contrast, ncol = 5, scales = "free") + geom_vline(data = ems_quantiles, aes(xintercept = lower), linetype = 2) + geom_vline(data = ems_quantiles, aes(xintercept = upper), linetype = 2) + geom_vline(data = ems_quantiles, aes(xintercept = estimate), color = "red", size = 1.5) + labs(x = "Estimate", y = "Frequency") + theme_pairs

