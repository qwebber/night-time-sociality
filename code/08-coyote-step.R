
## load libraries
library(data.table)
library(mgcv)
library(gamm4)
library(ggplot2)

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

## load coyote data
coyoteMR <- fread("../prepare-locs/output/NL-Provincial-Coyote-Telemetry_MIDRIDGE.csv")
coyoteLA <- fread("../prepare-locs/output/NL-Provincial-Coyote-Telemetry_LAPOILE.csv")
coyoteNP <- fread("../prepare-locs/output/NL-Provincial-Coyote-Telemetry_NOPENINS.csv")
coyote <- rbind(coyoteLA, coyoteMR, coyoteNP)


## rename vars to EASTING/NORTHING
setnames(coyote, c("x_proj", "y_proj"), c("EASTING", "NORTHING"))

coyote <- coyote[(lowEast < EASTING & EASTING < highEast) &
						 	(lowNorth < NORTHING & NORTHING < highNorth)]

## sl for coyote
step_length(
	coyote,
	coords = projCols,
	time = 'datetime',
	splitBy = c('id', 'yr'),
	moverate = TRUE
)



# Create lag and dif column names
lag.cols <- paste('lag', projCols, sep = '')

## calculate hour of day
coyote[, hr := hour(datetime)]

## add seasons
# Set seasons
coyote[doy >= 121 & doy <= 170, season := 'Spring']
coyote[doy >= 1 & doy <= 75, season := 'Winter']
coyote <- coyote[!(is.na(season))]
coyote <- coyote[!is.na(coyote$moveRate)]

## detect earliest loc for each individual
aa <- coyote[, min(doy), by = c("yr", "id", "season")]
aa$max <- coyote[, max(doy), by = c("yr", "id", "season")]$V1

## remove IDs with few steps
coyote[, .N, by = c("yr", "id", "season")]
coyote$idyrseason <- as.factor(paste(coyote$id,
																		 coyote$yr,
																		 coyote$season, sep = "_"))

## remove id-season-yr combinations with too few data
coyote <- coyote[idyrseason != "co_np0906_2010_Winter" &
								 	idyrseason != "co_np0904_2009_Spring" &
								 	idyrseason != "co_np1003_2012_Winter" &
								 	idyrseason != "co_mr1314_2013_Winter" &
								 	idyrseason != "co_mr1106_2011_Winter" &
								 	idyrseason != "co_mr1104_2011_Winter" &
								 	idyrseason != "co_mr1103_2011_Winter" &
								 	idyrseason != "co_mr1102_2011_Winter" &
								 	idyrseason != "co_mr0914_2009_Winter" &
								 	idyrseason != "co_mr0914_2009_Spring" &
								 	idyrseason != "co_mr0911_2009_Winter" &
								  idyrseason != "co_mr0910_2009_Winter" &
								 	idyrseason != "co_mr0909_2009_Winter" &
								 	idyrseason != "co_mr0908_2009_Winter" &
								  idyrseason != "co_mr0907_2009_Winter" &
								 	idyrseason != "co_mr0906_2009_Winter" &
								 	idyrseason != "co_mr0905_2009_Winter" &
								 	idyrseason != "co_mr0901_2009_Winter" &
								 	idyrseason != "co_lp0908_2009_Winter" &
								 	idyrseason != "co_lp0904_2010_Winter" &
								 	idyrseason != "co_lp0904_2009_Winter" &
								 	idyrseason != "co_lp0902_2009_Winter"]

coyoteSUM <- coyote[season == "Spring"]
coyoteWIN <- coyote[season == "Winter"]

## variable number of steps per hour; pool hrs with small sample sizes
coyoteWIN[, .N, by = "hr"]

coyoteWIN$hr[coyoteWIN$hr == 1] <- 0
coyoteWIN$hr[coyoteWIN$hr == 13] <- 14
coyoteWIN$hr[coyoteWIN$hr == 21] <- 22
coyoteWIN$hr[coyoteWIN$hr == 9] <- 10


a1 <- gamm4(moveRate ~ s(hr, bs = "cc") + log(doy) + as.factor(yr), random = ~(1|id), data = coyoteSUM)
summary(a1$mer)
summary(a1$gam)

a2 <- gamm4(moveRate ~ s(hr, bs = "cc") + log(doy) + as.factor(yr), random = ~(1|id), data = coyoteWIN)
summary(a2$mer)
summary(a2$gam)

coyoteSUM$pred <- predict(a1$gam)
coyoteWIN$pred <- predict(a2$gam)



coyALL <- rbind (coyoteSUM, coyoteWIN)


coyALL[, .N, by = c("hr", "season")]



aa <- coyALL[, mean(pred), by = c("season", "hr")]

write.csv(aa, "output/coyote-all-move.csv")

#### FIGURE 1 #####

a <- ggplot(data = aa[season == "Spring"],
						aes(hr, V1, color = season)) +
	geom_smooth(method = "gam") +
	xlab("Hour of day") +
	ylim(200, 650) +
	ylab("Predicted movement rate (m/hr)") +
	scale_color_manual(values = c("#e34a33")) +
	geom_vline(data = coyALL[season == "Spring"],
						 aes(xintercept = 6), lty = 2, color = "#e34a33") +
	geom_vline(data = coyALL[season == "Summer"],
						 aes(xintercept = 21), lty = 2, color = "#e34a33") +
	ggtitle("A) Spring") +
	theme(legend.position = "none",
				strip.background = element_rect(color = "black",
																				fill = "white",
																				size = 1),
				axis.title = element_text(size = 16, color = 'black'),
				axis.text.x = element_text(size = 12, color = 'black'),
				axis.text.y = element_text(size = 12, color = 'black'),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(colour = "black", fill=NA, size = 1))

b <- ggplot(aa[season == "Winter"], aes(hr, V1, color = season)) +
	geom_smooth(method = "gam") +
	xlab("Hour of day") +
	ylim(200, 650) +
	ylab("Predicted movement rate (m/hr)") +
	scale_color_manual(values = c("#2b8cbe")) +
	geom_vline(data = coyALL[season == "Winter"],
						 aes(xintercept = 7.5), lty = 2, color = "#2b8cbe") +
	geom_vline(data = coyALL[season == "Winter"],
						 aes(xintercept = 17.5), lty = 2, color = "#2b8cbe") +
	ggtitle("B) Winter") +
	theme(legend.position = "none",
				strip.background = element_rect(color = "black",
																				fill = "white",
																				size = 1),
				axis.title = element_text(size = 16, color = 'black'),
				axis.text.x = element_text(size = 12, color = 'black'),
				axis.text.y = element_text(size = 12, color = 'black'),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(colour = "black", fill=NA, size = 1))
