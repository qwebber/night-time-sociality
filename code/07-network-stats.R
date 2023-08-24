
## load packages
library(data.table)
library(ggplot2)
library(lme4)
library(performance)
library(emmeans)
library(Rmisc)

## load data
fogo <- fread("output/tod-networks.csv")
coefs <- fread("output/rdm-coefs.csv")
ems <- fread("output/rdm-ems.csv")

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
ggplot(data=emdat, aes(x=Cover,y=emmean, fill=`Time of Day`)) +
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


pairsW <- as.data.table(pairsW)


#### FIGURE 3 #####

theme_pairs <- theme(legend.position = 'none',
											strip.background = element_rect(color = "black",
																											fill = "white",
																											size = 1),
											strip.text = element_text(size = 10,
																								color = "black"),
											axis.text = element_text(size = 10,
																							 color = "black"),
											axis.title = element_text(size = 14),
										  plot.title = element_text(size = 10),
											panel.grid.minor = element_blank(),
											panel.background = element_blank(),
											panel.border = element_rect(color = "black", fill=NA, size = 1))



c1 <- ggplot() +
	geom_histogram(data = ems[contrast == "day Forest - night Forest"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Day Forest - Night Forest"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "day Forest - night Forest"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "day Forest - night Forest"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("A) Day forest - Night forest") +
	theme_pairs

c2 <- ggplot() +
	geom_histogram(data = ems[contrast == "day Forest - day Lichen"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Day Forest - Day Lichen"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "day Forest - day Lichen"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "day Forest - day Lichen"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("B) Day forest - Day Lichen") +
	theme_pairs

c3 <- ggplot() +
	geom_histogram(data = ems[contrast == "day Forest - night Lichen"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Day Forest - Night Lichen"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "day Forest - night Lichen"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "day Forest - night Lichen"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("C) Day forest - Night Lichen") +
	theme_pairs

c4 <- ggplot() +
	geom_histogram(data = ems[contrast == "day Forest - day openMove"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Day Forest - Day Rocky barrens"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "day Forest - day openMove"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "day Forest - day openMove"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("D) Day forest - Day Rocky barrens") +
	theme_pairs

c5 <- ggplot() +
	geom_histogram(data = ems[contrast == "day Forest - night openMove"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Day Forest - Night Rocky barrens"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "day Forest - night openMove"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "day Forest - night openMove"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("E) Day forest - Night Rocky barrens") +
	theme_pairs

c6 <- ggplot() +
	geom_histogram(data = ems[contrast == "night Forest - day Lichen"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Night Forest - Day Lichen"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "night Forest - day Lichen"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "night Forest - day Lichen"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("F) Night forest - Day Lichen") +
	theme_pairs

c7 <- ggplot() +
	geom_histogram(data = ems[contrast == "night Forest - night Lichen"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Night Forest - Night Lichen"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "night Forest - night Lichen"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "night Forest - night Lichen"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("G) Night forest - Night Lichen") +
	theme_pairs

c8 <- ggplot() +
	geom_histogram(data = ems[contrast == "night Forest - day openMove"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Night Forest - Day Rocky barrens"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "night Forest - day openMove"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "night Forest - day openMove"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("H) Night forest - Day Rocky barrens") +
	theme_pairs

c9 <- ggplot() +
	geom_histogram(data = ems[contrast == "night Forest - night openMove"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Night Forest - Night Rocky barrens"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "night Forest - night openMove"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "night Forest - night openMove"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("I) Night forest - Night Rocky barrens") +
	theme_pairs

c10 <- ggplot() +
	geom_histogram(data = ems[contrast == "day Lichen - night Lichen"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Day Lichen - Night Lichen"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "day Lichen - night Lichen"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "day Lichen - night Lichen"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("J) Day Lichen - Night Lichen") +
	theme_pairs

c11 <- ggplot() +
	geom_histogram(data = ems[contrast == "day Lichen - day openMove"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Day Lichen - Day Rocky barrens"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "day Lichen - day openMove"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "day Lichen - day openMove"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("K) Day Lichen - Day Rocky barrens") +
	theme_pairs

c12 <- ggplot() +
	geom_histogram(data = ems[contrast == "day Lichen - night openMove"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Day Lichen - Night Rocky barrens"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "day Lichen - night openMove"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "day Lichen - night openMove"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("L) Day Lichen - Night Rocky barrens") +
	theme_pairs

c13 <- ggplot() +
	geom_histogram(data = ems[contrast == "night Lichen - day openMove"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Night Lichen - Day Rocky barrens"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "night Lichen - day openMove"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "night Lichen - day openMove"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("M) Night Lichen - Day Rocky barrens") +
	theme_pairs

c14 <- ggplot() +
	geom_histogram(data = ems[contrast == "night Lichen - night openMove"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Night Lichen - Night Rocky barrens"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "night Lichen - night openMove"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "night Lichen - night openMove"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("N) Night Lichen - Night Rocky barrens") +
	theme_pairs

c15 <- ggplot() +
	geom_histogram(data = ems[contrast == "day openMove - night openMove"],
								 aes(estimate)) +
	geom_vline(data = pairsW[contrast == "Day Rocky barrens - Night Rocky barrens"],
						 aes(xintercept = estimate), color = 'red', size = 1.5) +
	geom_vline(data = ems[contrast == "day openMove - night openMove"],
						 aes(xintercept = quantile(estimate, 0.025)), lty = 2) +
	geom_vline(data = ems[contrast == "day openMove - night openMove"],
						 aes(xintercept = quantile(estimate, 0.975)), lty = 2) +
	xlab("Estimate") +
	ylab("Frequency") +
	ggtitle("O) Day Rocky barrens - Night Rocky barrens") +
	theme_pairs


png("figures/fig3.png",
		width = 10000,
		height = 6000,
		res = 600,
		units = "px")
grid.arrange(c1, c2, c3, c4, c5,
						 c6, c7, c8, c9, c10,
						 c11, c12, c13, c14, c15,
						 nrow = 3, ncol = 5)
dev.off()
