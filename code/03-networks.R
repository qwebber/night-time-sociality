
library(data.table)
library(spatsoc)

fogo <- fread("output/fogo-gsp.csv")

###### GENERATE NETWORKS FOR OBSERVED DATA ######

fogo <- group_times(fogo, datetime = 'datetime', threshold = '5 minutes')

fogo <- group_pts(
	fogo,
	threshold = 50,
	splitBy = c('yr', 'season', 'tod'),
	timegroup = 'timegroup',
	id = 'id',
	coords = c('x_proj', 'y_proj')
)

### Calculate network metrics
source("functions/dynamic_network.R")

nets <- dynamic_network(fogo,
												id = 'id',
												by = c('yr', 'season', 'tod'))

fwrite(nets, "output/tod-networks.csv")

source("functions/get_graph.R")

nets <- get_graph(fogo,
												id = 'id' ) #,
												by = c('yr', 'season', 'tod'))
