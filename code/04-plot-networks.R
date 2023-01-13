

library(data.table)
library(spatsoc)
library(igraph)

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

## generate networks

## summer 2017 night
fogo17_night_summer <- fogo[season == "summer" & tod == "night" & yr == "2017"]
d <- data.table::dcast(data = fogo17_night_summer ,group ~ id,
											 value.var = 'group',
											 fun.aggregate = length)

gbi_df <- data.matrix(d[, !'group', with = FALSE])

rownames(gbi_df) <- d$group

gbi.net_df <- asnipe::get_network(gbi_df, data_format = "GBI",
																	association_index = "SRI")
gbi.net_df[lower.tri(gbi.net_df)] <- NA
diag(gbi.net_df) <- NA

gbi.sum17_night <- graph.adjacency(gbi.net_df,
															 mode = "undirected",
															 diag = FALSE,
															 weighted = TRUE)

## summer 2017 day
fogo17_day_summer <- fogo[season == "summer" & tod == "day" & yr == "2017"]
d <- data.table::dcast(data = fogo17_day_summer ,group ~ id,
											 value.var = 'group',
											 fun.aggregate = length)

gbi_df <- data.matrix(d[, !'group', with = FALSE])

rownames(gbi_df) <- d$group

gbi.net_df <- asnipe::get_network(gbi_df, data_format = "GBI",
																	association_index = "SRI")
gbi.net_df[lower.tri(gbi.net_df)] <- NA
diag(gbi.net_df) <- NA

gbi.sum17_day <- graph.adjacency(gbi.net_df,
																	 mode = "undirected",
																	 diag = FALSE,
																	 weighted = TRUE)


## winter 2018 night
fogo18_night_winter <- fogo[season == "winter" & tod == "night" & yr == "2018"]
d <- data.table::dcast(data = fogo18_night_winter ,group ~ id,
											 value.var = 'group',
											 fun.aggregate = length)

gbi_df <- data.matrix(d[, !'group', with = FALSE])

rownames(gbi_df) <- d$group

gbi.net_df <- asnipe::get_network(gbi_df, data_format = "GBI",
																	association_index = "SRI")
gbi.net_df[lower.tri(gbi.net_df)] <- NA
diag(gbi.net_df) <- NA

gbi.win18_night <- graph.adjacency(gbi.net_df,
																	 mode = "undirected",
																	 diag = FALSE,
																	 weighted = TRUE)

## winter 2017 day
fogo18_day_winter <- fogo[season == "winter" & tod == "day" & yr == "2018"]
d <- data.table::dcast(data = fogo18_day_winter ,group ~ id,
											 value.var = 'group',
											 fun.aggregate = length)

gbi_df <- data.matrix(d[, !'group', with = FALSE])

rownames(gbi_df) <- d$group

gbi.net_df <- asnipe::get_network(gbi_df, data_format = "GBI",
																	association_index = "SRI")
gbi.net_df[lower.tri(gbi.net_df)] <- NA
diag(gbi.net_df) <- NA

gbi.win18_day <- graph.adjacency(gbi.net_df,
																 mode = "undirected",
																 diag = FALSE,
																 weighted = TRUE)

ggplot() +
	geom_sf(data = islands, fill = '#e7dddd') +
	geom_edges(data = edge_DT1, aes(#size = weight,
		x = mean_x_1,
		y = mean_y_1,
		xend = mean_x_2,
		yend = mean_y_2), alpha = 0.5) +
	geom_nodes(data = centroid_DT1,
						 aes(x = mean_x,
						 		y = mean_y,
						 		color = as.factor(membership)),
						 size = 1) +
	ggtitle("A) 2017") +
	labs(x = 'Longitude', y = 'Latitude') +
	themeMap
