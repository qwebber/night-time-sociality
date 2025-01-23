
## load libraries
library(data.table)
library(mgcv)
library(gamm4)


## load data
fogo <- fread("output/fogo-gsp.csv")
NL <- fread("../prepare-locs/output/NL-Provincial-Caribou-Telemetry_MIDRIDGE.csv")

# Max moverate
maxMoveRate <- 30000

### Projection ----
projCols <- c('EASTING', 'NORTHING')

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

source("functions/step_length.R")
source("functions/internal.R")

## assign hour
setnames(fogo, c("x_proj", "y_proj"), c("EASTING", "NORTHING"))
fogo[, hr := hour(datetime)]

setnames(NL, c("x_proj", "y_proj"), c("EASTING", "NORTHING"))
NL[, hr := hour(datetime)]

## estimate step length
step_length(
	fogo,
	coords = projCols,
	time = 'datetime',
	splitBy = c('id', 'yr'),
	moverate = TRUE
)

step_length(
	NL,
	coords = projCols,
	time = 'datetime',
	splitBy = c('id', 'yr'),
	moverate = TRUE
)

## add seasons
# Set seasons
fogo[doy >= 121 & doy <= 170, season := 'Spring']
fogo[doy >= 1 & doy <= 75, season := 'Winter']
fogo <- fogo[!(is.na(season))]
fogo <- fogo[!is.na(fogo$moveRate)]

NL[doy >= 121 & doy <= 170, season := 'Spring']
NL[doy >= 1 & doy <= 75, season := 'Winter']
NL <- NL[!(is.na(season))]
NL <- NL[!is.na(NL$moveRate)]

## subset seasons
fogoSUM <- fogo[season == "Spring"]
fogoWIN <- fogo[season == "Winter"]

NLSUM <- NL[season == "Spring"]
NLWIN <- NL[season == "Winter"]


## run GAMs
a1 <- gamm4(moveRate ~ s(hr, bs = "cc") + log(doy) + as.factor(yr), random = ~(1|id), data = fogoSUM)
a2 <- gamm4(moveRate ~ s(hr, bs = "cc") + log(doy) + as.factor(yr), random = ~(1|id), data = fogoWIN)

a3 <- gamm4(moveRate ~ s(hr, bs = "cc") + log(doy) + as.factor(yr), random = ~(1|id), data = NLSUM)
a4 <- gamm4(moveRate ~ s(hr, bs = "cc") + log(doy) + as.factor(yr), random = ~(1|id), data = NLWIN)

## summary of models
summary(a1$gam) ## Fogo summer
summary(a2$gam) ## Fogo winter
summary(a3$gam) ## MR summer
summary(a4$gam) ## MR winter

## generate predictive values
fogoSUM$pred <- predict(a1$gam)
fogoWIN$pred <- predict(a2$gam)
NLSUM$pred <- predict(a3$gam)
NLWIN$pred <- predict(a4$gam)


## combine seasons back together
fogoALL <- rbind(fogoSUM, fogoWIN)
NLALL <- rbind(NLSUM, NLWIN)

aa <- fogoALL[, mean(pred), by = c("season", "hr")]
bb <- NLALL[, mean(pred), by = c("season", "hr")]

fwrite(aa, "output/caribou-FO-move.csv")
fwrite(bb, "output/caribou-NL-move.csv")

