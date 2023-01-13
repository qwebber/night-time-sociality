


### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'spatsoc', 'amt',
					'tidyverse', 'lubridate', 'raster', 'sp')
lapply(libs, require, character.only = TRUE)

## load data
DT <- fread("output/fogo-gsp.csv")
lcFogo <- raster("../fogo_coyote_repeat/data/raw-data/Landcover/FogoSDSS_RS.tif") # This is a landcover map with different habitat types
Legend <- fread("../fogo_coyote_repeat/data/raw-data/Landcover/Legend.csv")

## order by datetime
DT <- DT[order(DT$datetime),]
DT <- DT[!is.na(datetime)]

## Variables
crs = CRS("+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## read in functions
source("functions/ExtractPoints.R")

## extract habitat type at end step
DT[, Value := ExtractPoints(matrix(c(x_proj, y_proj), ncol = 2),
														raster = lcFogo)]

## convert NAs to unavailable habitat
DT$Value[is.na(DT$Value)] <- 10

## rename habitat types
DT <- merge(DT, Legend, by = 'Value')

## check number of fixes by habitat type:
DT[, .N, by = "Cover"]

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

fwrite(DT, "output/fogo-gsp.csv")


