
library(data.table)
library(lubridate)

mr <- fread("../prepare-locs/output/NL-Provincial-Caribou-Telemetry_MIDRIDGE.csv", drop = c("SPECIES", "EPSG_CODE", "Map_Quality",
							 				 "COLLAR_FILE_ID", "EXCLUDE", "VENDOR_CL", "DOP",
							 				 "NAV", "VALIDATED", "LOCQUAL", "COLLAR_ID",
							 				 "AGE", "Fix_Time_Delta", "V1", "FIX_ID"))

mr[, hr := hour(datetime)]

# assign season
mr[doy >= 15 & doy <= 63, season := 'winter']
mr[doy >= 122 & doy <= 170, season := 'spring']
mr <- mr[!(is.na(season))]

mrW <- mr[season == 'winter']
mrS <- mr[season == 'spring']

## average sunrise and sunset in winter
## sunrise: Jan 15 = 7:57am; March 4 = 6:41am (average 7:19am)
## sunset: Jan 15 = 4:35pm; March 4 = 5:54pm (average 5:14pm)
mrW[hr >= 17 | hr <= 7, tod := 'night']
mrW[hr <= 17 & hr >= 7, tod := 'day']

## average sunrise and sunset in winter
## sunruse: May 1 = 5:44am; June 19 = 4:58am (average 5:22am)
## sunset: May 1 = 8:24pm; June 19 = 9:17pm (average 8:50pm)
mrS[hr >= 2030 | hr <= 6, tod := 'night']
mrS[hr <= 2030 & hr >= 6, tod := 'day']

mr <- rbind(mrS, mrW)

mr$IDYr <-paste(mr$id, mr$yr, sep = "_")

## remove individuals with insufficient data or those that died
mr[, N:= .N, by = c('id','yr', 'season', 'tod')]
mr <- mr[N > 100]

fwrite(mr, "output/mr-gsp.csv")
