
## load libraries
library(data.table)

# Filter by bounds
lowEast <- 0; highEast <- 800000
lowNorth <- 5200000; highNorth <- 6000000

# Max moverate
maxMoveRate <- 30000

### Projection ----
projCols <- c('EASTING', 'NORTHING')

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

source("functions/step_length.R")
source("functions/internal.R")

## load data
coyoteMR <- fread("../prepare-locs/output/NL-Provincial-Coyote-Telemetry_MIDRIDGE.csv")

setnames(coyoteMR, c("x_proj", "y_proj"), c("EASTING", "NORTHING"))

coyoteMR <- coyoteMR[(lowEast < EASTING & EASTING < highEast) &
						 	(lowNorth < NORTHING & NORTHING < highNorth)]

step_length(
	coyoteMR,
	coords = projCols,
	time = 'datetime',
	splitBy = c('id', 'yr'),
	moverate = TRUE
)

## calculate hour of day
coyoteMR[, hr := hour(datetime)]

## add seasons
# Set seasons
coyoteMR[doy > 121 & doy < 244, season := 'summer']

coyoteMR <- coyoteMR[!(is.na(season))]
coyoteMR <- coyoteMR[!is.na(coyoteMR$moveRate)]

a1 <- gam(moveRate ~ s(hr), data = coyoteMR)
summary(a1)


coyoteMR$pred <- predict(a1)

ggplot(coyoteMR) +
	geom_point(aes(pred,moveRate))

ggplot(coyoteMR, aes(hr, pred)) +
	geom_point() +
	geom_line() +
	ggtitle("Coyote activity between May 1 and Sept 1") +
	ylab("Time of day") +
	xlab("Predicted movement length (m/hr)") +
	geom_vline(xintercept = 6, lty = 2, color = "red") +
	geom_vline(xintercept = 22, lty = 2, color = "red") +
	theme_bw()

