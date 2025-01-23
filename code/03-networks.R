
library(data.table)
library(spatsoc)

fogo <- fread("output/fogo-gsp.csv")
fogo$herd <- "FOGO"
mr <- fread("output/mr-gsp.csv")

all <- rbind(fogo, mr, fill = T)

###### GENERATE NETWORKS FOR OBSERVED DATA ######

all <- group_times(all, datetime = 'datetime', threshold = '5 minutes')

all <- group_pts(
	all,
	threshold = 50,
	splitBy = c('yr', 'season', 'tod', 'Cover', 'herd'),
	timegroup = 'timegroup',
	id = 'id',
	coords = c('x_proj', 'y_proj')
)

### Calculate network metrics
source("functions/dynamic_network.R")

nets <- dynamic_network(all,
												id = 'id',
												by = c('yr', 'season', 'tod', 'Cover', 'herd'))

fwrite(nets, "output/tod-networks.csv")
