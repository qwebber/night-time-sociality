


### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'spatsoc',
					'tidyverse', 'lubridate', 'raster', 'sp')
lapply(libs, require, character.only = TRUE)

## load data
DT <- fread("output/mr-gsp.csv")
# Landcover
lc <- raster("../nl-landcover/input/FINAL_PRODUCT/FINAL_RC.tif")
legend <- fread('../caribou-specialization-sociality/input/landcover/Legend.csv')


## order by datetime
DT <- DT[order(DT$datetime),]
DT <- DT[!is.na(datetime)]

## Variables
crs = CRS("+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## read in functions
source("functions/ExtractPoints.R")

## extract habitat type at end step
DT[, Value := ExtractPoints(matrix(c(x_proj, y_proj), ncol = 2),
														raster = lc)]

## convert NAs to unavailable habitat
DT$Value[is.na(DT$Value)] <- 10

## rename habitat types
DT <- merge(DT, legend, by = 'Value')

## check number of fixes by habitat type:
DT[, .N, by = c("Cover", "season")]

## combine habitat types
DT$Cover[DT$Cover == "Wetland"] <- "openMove"
DT$Cover[DT$Cover == "Rocky"] <- "openMove"
DT$Cover[DT$Cover == "Water"] <- "openMove"
DT$Cover[DT$Cover == "Anthro"] <- "openMove"
DT$Cover[DT$Cover == "ConiferForest"] <- "Forest"
DT$Cover[DT$Cover == "MixedWood"] <- "Forest"
DT$Cover[DT$Cover == "ConiferScrub"] <- "Forest"
DT$Cover[DT$Cover == "Broadleaf"] <- "Forest"

DT <- DT[Cover != "NotAvail"]

fwrite(DT, "output/mr-gsp.csv")
