#' Generate bipartite graph from an edge list selection.
#' 
#' @param df A tibble or data frame.
#' @param edge_list Selected columns as a character vector.
#' @returns An igraph graph with a bipartite.mapping attribute.
#' @examples 
#' el <- tibble(c1 = c("A", "A", "B", "B", "C", "C", "D"), c2 = c(1, 2, 2, 3, 3, 4, 4))
#' graph_bi <- my_bipartite_graph(el, c("c1", "c2"))
#' @export
bipartite_graph <- function(df, edge_list) {
  graph_obj <- df %>%
    dplyr::select(tidyselect::all_of(edge_list))%>%
    igraph::graph.data.frame(directed = FALSE)
  igraph::V(graph_obj)$type <- igraph::bipartite.mapping(graph_obj)$type
  graph_obj
}

#' Generate tripartite graph from an edge list selection.
#' 
#' @param df A tibble or data frame.
#' @param edge_list Selected columns as a character vector.
#' @returns An igraph graph with tripartite graph attributes.
#' @export
tripartite_graph <- function(df, edge_list){
  mtx <- df %>%
    dplyr::select(tidyselect::all_of(edge_list))%>%
    as.matrix()
  el <- rbind(mtx[,1:2], mtx[,2:3])
  graph_tri <- igraph::graph_from_edgelist(el, directed = FALSE)
  layers <- ceiling(match(V(graph_tri)$name, mtx)/nrow(mtx))
  l <- igraph::layout_with_sugiyama(graph_tri, layers)
  graph_tri$mtx <- mtx 
  graph_tri$el <- el
  graph_tri$l <- l 
  graph_tri$dup <- duplicated(graph_tri$el) | duplicated(graph_tri$el,fromLast = TRUE )
  graph_tri
}