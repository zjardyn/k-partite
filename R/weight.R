#' Generate edge weights based on number of times the edge exists.  
#'
#' @param graph_bi A bipartite graph object.
#' @returns A vector. Use it to set the weight attribute of the graph object.
#' @export
edge_weights_n <- function(graph_bi){
  el <- igraph::get.edgelist(graph_bi)
  el_df <- tibble::tibble(el = paste(el[,1], el[,2])) %>%
    dplyr::group_by(el) %>%
    dplyr::summarize(n = n())
  el_s <- igraph::get.edgelist(igraph::simplify(graph_bi))
  el_v <- paste(el_s[,1], el_s[,2])
  el_df[match(el_v, el_df$el),] %>%
    pull(n)
}
