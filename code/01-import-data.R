

library(data.table)

fogo <- fread("../prepare-locs/output/NL-Fogo-Caribou-Telemetry.csv")

fogo$IDYr <-paste(fogo$id, fogo$yr, sep = "_")

## remove individuals with insufficient data or those that died
fogo <- fogo[IDYr != "FO2016006_2017" & ## not enough fixes
						 	IDYr != "FO2016006_2018" & ## not enough fixes
						 	IDYr != "FO2017006_2019" & ## not enough fixes
						 	IDYr != "FO2016015_2017" & ## not enough fixes
						 	id != "FO2017013" & ## Dead animal
						 	id != "FO2016001" &   ## Dead animal
						 	id != "FO2016011"]  ## Indian Island

fogo[, hr := hour(datetime)]

# assign season
fogo[doy >= 15 & doy <= 63, season := 'winter']
fogo[doy >= 122 & doy <= 170, season := 'spring']
fogo <- fogo[!(is.na(season))]

fogoW <- fogo[season == 'winter']
fogoS <- fogo[season == 'spring']

## average sunrise and sunset in winter
## sunrise: Jan 15 = 7:57am; March 4 = 6:41am (average 7:19am)
## sunset: Jan 15 = 4:35pm; March 4 = 5:54pm (average 5:14pm)
fogoW[hr >= 17 | hr <= 7, tod := 'night']
fogoW[hr <= 17 & hr >= 7, tod := 'day']

## average sunrise and sunset in winter
## sunruse: July 15 = 5:16am; Sept 1 = 6:23am (average 5:49am)
## sunset: July 15 = 9:08pm; Sept 1 = 7:49pm (average 8:29pm)
fogoS[hr >= 2030 | hr <= 6, tod := 'night']
fogoS[hr <= 2030 & hr >= 6, tod := 'day']

fogo <- rbind(fogoS, fogoW)

ids <- fogo[, .N, by = c('id','yr', 'season', 'tod')]
ids[N < 100]

fwrite(fogo, "output/fogo-gsp.csv")

