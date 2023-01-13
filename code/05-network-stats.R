

## install packages
install.packages("data.table")
install.packages("ggplot2")
install.packages("lme4")
install.packages("performance")

## load packages
library(data.table)
library(ggplot2)
library(lme4)
library(performance)

## load data
fogo <- fread("output/tod-networks.csv")

## your working directory may be something like this...
## I just used desktop, but the file path will be whereever you save the file
fogo <- fread("/Users/maryatkinson/Desktop/tod-networks.csv")

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

## regular linear regression
model1 <- lm(logStrength ~ tod + season, data = fogo)
summary(model1)

## simple mixed model
model2 <- lmer(logStrength ~ tod + season + (1|ID), data = fogo)
summary(model2)

## more complicated mixed model
model3 <- lmer(logStrength ~ tod * season + (1|yr) + (1|ID), data = fogo)
summary(model3)

## model checks
check_model(model3)
model_performance(model3)

## plot results (we can work on this)
ggplot(fogo) +
	geom_boxplot(aes(tod, logStrength)) +
	facet_wrap(~season)
