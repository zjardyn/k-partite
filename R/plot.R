#' @export
ggbipart <- function(graph_bi, edge_colour = NULL, edge_weight = NULL, horizontal = FALSE){
  p <- graph_bi %>%
    ggraph::ggraph(layout = "bipartite") + 
      ggraph::geom_node_point() + 
      ggraph::geom_edge_link(ggplot2::aes(colour = edge_colour, edge_width = edge_weight)) +
      ggraph::geom_node_text(ggplot2::aes(label = name), hjust = "outward", vjust = "outward")
  if(horizontal == TRUE){
    p + coord_flip() + scale_y_reverse() + scale_x_reverse()
  } else {
    p
  }
}
