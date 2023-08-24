### Randomizations ====

### Packages ----
libs <- c('data.table', 'ggplot2', 'gridExtra',
					'lme4', 'spatsoc', 'igraph', 'gmodels')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- fread("output/fogo-gsp.csv")

### Variables ----
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

### Functions ----
source("functions/dynamic_network.R")

### Proximity Based Social Networks ----
# Need to allocate columns since reading from .Rds
if (truelength(locs) == 0) alloc.col(locs)

group_times(locs, datetime = 'datetime', threshold = '5 minutes')

group_pts(
	locs,
	threshold = 50,
	splitBy = c('yr', 'season', 'tod', 'Cover'),
	timegroup = 'timegroup',
	id = 'id',
	coords = c('x_proj', 'y_proj')
)

source("functions/dynamic_network.R")

### Randomizations ----
# Number of iterations
N <-  1000

lsDynNets <- lapply(1:N, FUN = function(i) {
	locsRandom <-
		randomizations(
			locs,
			id = 'id',
			type = 'trajectory',
			splitBy = c('yr', 'season', 'tod', 'Cover'),
			group = 'group',
			datetime = 'datetime',
			iterations = 1,
			coords = c('x_proj', 'y_proj')
		)

	group_times(locsRandom, datetime = 'randomdatetime', threshold = '5 minutes')

	group_pts(
		locsRandom,
		threshold = 50,
		splitBy = c('yr', 'season', 'tod', 'Cover'),
		timegroup = 'timegroup',
		id = 'id',
		coords = c('x_proj', 'y_proj')
	)

	print(i)

	return(dynamic_network(locsRandom, id = 'id',
												 c(c('yr', 'season', 'tod', 'Cover')))[, iteration := i])
}
)

dynNets <- rbindlist(lsDynNets)

### Output ----
saveRDS(dynNets, 'output/rdmNets-1000.RDS')
