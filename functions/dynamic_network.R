#' Dynamic network
#'
#' @inheritParams hr_network
#'
#' @return Graph strength for each individual.
#' @export
#'
#'
dynamic_network <- function(DT = NULL, id = NULL, by = NULL) {

  if (is.null(DT) | is.null(id)) {
    stop('DT, and id must be provided')
  }


  DT[, {
    d <- data.table::dcast(.SD, group ~ id,
                           value.var = 'group',
                           fun.aggregate = length)

    gbi_df <- data.matrix(d[, !'group', with = FALSE])

    rownames(gbi_df) <- d$group

    gbi.net_df <- asnipe::get_network(gbi_df, data_format = "GBI",
                                      association_index = "SRI")

    gbi.grph_df <- igraph::graph_from_adjacency_matrix(gbi.net_df,
                                                       mode = "undirected",
                                                       diag = FALSE,
                                                       weighted = TRUE)



    list(
      strength = igraph::strength(gbi.grph_df),
      ID = names(igraph::degree(gbi.grph_df))
    )
  }, by = by]
}
