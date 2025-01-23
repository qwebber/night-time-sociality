

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

## load bear data
bearMR <- fread("../prepare-locs/output/2024-09-24_NL-Provincial-Bear-Telemetry_MIDRIDGE.csv")
bearLA <- fread("../prepare-locs/output/2024-09-24_NL-Provincial-Bear-Telemetry_LAPOILE.csv")
bearNP <- fread("../prepare-locs/output/2024-09-24_NL-Provincial-Bear-Telemetry_NOPENINS.csv")
bear <- bearMR



## rename vars to EASTING/NORTHING
setnames(bear, c("x_proj", "y_proj"), c("EASTING", "NORTHING"))

bear <- bear[(lowEast < EASTING & EASTING < highEast) &
								 	(lowNorth < NORTHING & NORTHING < highNorth)]

## sl for bear
step_length(
	bear,
	coords = projCols,
	time = 'datetime',
	splitBy = c('id', 'yr'),
	moverate = TRUE
)



# Create lag and dif column names
lag.cols <- paste('lag', projCols, sep = '')

## calculate hour of day
bear[, hr := hour(datetime)]

## add seasons
# Set seasons
bear[doy >= 121 & doy <= 170, season := 'Spring']
bear[doy >= 1 & doy <= 75, season := 'Winter']
bear <- bear[!(is.na(season))]
bear <- bear[!is.na(bear$moveRate)]

## detect earliest loc for each individual
aa <- bear[, min(doy), by = c("yr", "id", "season")]
aa$max <- bear[, max(doy), by = c("yr", "id", "season")]$V1

## remove IDs with few steps

bear$idyrseason <- as.factor(paste(bear$id,
																		 bear$yr,
																		 bear$season, sep = "_"))

bear[, pts := .N, by = c("idyrseason")]

## remove id-season-yr combinations with too few data
bear <- bear[pts > 75]

bearSUM <- bear[season == "Spring"]
bearSUM <- bearSUM[yr != "2008" & yr != "2013"]
unique(bearSUM$id)

a1 <- gamm4(moveRate ~ s(hr, bs = "cc") + log(doy) + as.factor(yr), random = ~(1|id), data = bearSUM)
summary(a1$mer)
summary(a1$gam)


bearSUM$pred <- predict(a1$gam)

aa <- bearSUM[, mean(pred), by = c("season", "hr")]

write.csv(aa, "output/bear-all-move.csv")

#### FIGURE 1 #####

ggplot(data = aa[season == "Spring"],
						aes(hr, V1, color = season)) +
	geom_smooth(method = "gam") +
	xlab("Hour of day") +
	ylim(0, 650) +
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

