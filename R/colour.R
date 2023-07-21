#' Generate a colour vector for an edge-side selection of a bipartite graph object.
#'
#' @param graph_bi A bipartite graph object.
#' @param edge_nodes A numerical value of 1 or 2, specifying the side of the edge to colour.
#' @returns A vector. Use it to set the colour attribute of the graph object.
#' @examples
#' library(tibble)
#' library(igraph)
#' el <- tibble(c1 = c("A", "A", "B", "B", "C", "C", "D"),c2 = c(1, 2, 2, 3, 3, 4, 4))
#' graph_bi <- my_bipartite_graph(el, c("c1", "c2"))
#' E(graph_bi)$colour <- colour_edges_bi(graph_bi, 1)
#' @export
colour_bi <- function(graph_bi, edge_nodes = 1){
  nodes <- unique(igraph::get.edgelist(graph_bi)[,edge_nodes])
  edge_colours <- vector(mode = "character", length = igraph::ecount(graph_bi))
  for(node in nodes){
    edge_colours[igraph::E(graph_bi)[.inc(node)]] <- node
  }
  edge_colours
}
#' Generate a colour vector for a node selection of a bipartite graph object.
#'
#' @param graph_bi A bipartite graph object.
#' @param nodes_s A character vector of selection(s).
#' @param binary If colours should be based on TRUE/FALSE.
#' @returns A vector. Use it to set the colour attribute of the graph object.
#' @examples
#' library(tibble)
#' library(igraph)
#' el <- tibble(c1 = c("A", "A", "B", "B", "C", "C", "D"),c2 = c(1, 2, 2, 3, 3, 4, 4))
#' graph_bi <- my_bipartite_graph(el, c("c1", "c2"))
#' E(graph_bi)$colour <- colour_edges_bi_selection_bi_2(graph_bi, c("A", "B"))
#' @export
colour_edge_selection <- function(graph_bi, nodes_s, binary = TRUE){
  nodes_s <- nodes_s %>%
    paste(collapse = "|")
  nodes <- igraph::get.edgelist(graph_bi) %>%
    as.vector() %>%
    unique()
  idx <- stringr::str_detect(nodes, nodes_s)
  matched_nodes <- nodes[idx]
  if(binary == TRUE){
     edge_colours <- rep(FALSE, igraph::ecount(graph_bi))
    for(node in matched_nodes){
      edge_colours[igraph::E(graph_bi)[.inc(node)]] <- TRUE
    }
    edge_colours
  } else {
    edge_colours <- rep("Other", igraph::ecount(graph_bi))
    for(node in matched_nodes){
      edge_colours[igraph::E(graph_bi)[.inc(node)]] <- node
    }
    edge_colours
  }
}
#' Colour tripartite graph by outer edge of nodes.
#' @param graph_tri A tripartite graph object.
#' @param rev What direction to colour in, default is lhs (FALSE).
#' @returns A vector, use it to colour plots.
#' @export
# TODO:! `pattern` must be a string, not an integer.
# TODO: sometimes whole colourscheme fails on rhs
colour_tri <- function(graph_tri, rev = FALSE){
  colours <- rep(NA, ecount(graph_tri))
  if(rev == TRUE){
    for (node in unique(unlist(graph_tri$mtx))) {
      node_edges <- E(graph_tri)[.inc(node)]
      colours[node_edges] <- node
    }
    colours
  } else {
    for (node in rev(unique(unlist(graph_tri$mtx)))) {
      node_edges <- E(graph_tri)[.inc(node)]
      colours[node_edges] <- node
    }
    colours
  }
}
#' @export
colour_tri_path3 <- function(graph_tri, mtx, edge_selection = 1){
  mtx <- graph_tri$mtx
  colours <- vector(length = ecount(graph_tri))
  for(i in seq(nrow(mtx))){
     colours[igraph::E(graph_tri)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]] <- mtx[i,edge_selection]
  }
  colours
}

#' @export
colour_tri_path_dup_s <- function(graph_tri, direction = "lhs") {
  graph_tri_s <- igraph::simplify(graph_tri)
  if (direction == "rhs"){
    # rhs
    if (sum(graph_tri$dup) != 0 && length(get_dup_edges(graph_tri, direction = "rhs")) != 0) {
      dup <- get_dup_edges(graph_tri, direction = "rhs", sep = ';')
      dup_split <- str_split(dup, " ")
      colour_s <- btvis::colour_tri_path3(graph_tri_s, edge_selection = 3)
      for (i in seq(ecount(graph_tri_s))){
        for (j in seq(length(dup_split))){
          edge <- paste(dup_split[[j]][1], dup_split[[j]][2])
          edge_list_i <- get.edgelist(graph_tri_s)[i,]
          edge_list_i_p <- paste(edge_list_i[1], edge_list_i[2])
          edge_list_i_p_r <- paste(edge_list_i[2], edge_list_i[1])
          if (edge_list_i_p == edge | edge_list_i_p_r == edge){
            colour_s[i] <- dup[j]
          }
        }
      }
    } else {
      colour_s <- colour_tri_path3(graph_tri_s, edge_selection = 3)
    }
  } else if (direction == "lhs"){
    # lhs
    if (sum(graph_tri$dup) != 0 && length(get_dup_edges(graph_tri, direction = "lhs")) != 0) {
      dup <- get_dup_edges(graph_tri, direction = "lhs", sep = ';')
      dup_split <- str_split(dup, " ")
      colour_s <- btvis::colour_tri_path3(graph_tri_s, edge_selection = 1)
      for (i in seq(ecount(graph_tri_s))){
        for (j in seq(length(dup_split))){
          edge <- paste(dup_split[[j]][2], dup_split[[j]][3])
          edge_list_i <- get.edgelist(graph_tri_s)[i,]
          edge_list_i_p <- paste(edge_list_i[1], edge_list_i[2])
          edge_list_i_p_r <- paste(edge_list_i[2], edge_list_i[1])
          if (edge_list_i_p == edge | edge_list_i_p_r == edge){
            colour_s[i] <- dup[j]
          }
        }
      }
    } else {
      colour_s <- colour_tri_path3(graph_tri_s, edge_selection = 1)
    }
  } else {
    stop('edge_selection is either "rhs" or "lhs".')
  }
  colour_s
}

#' @export
colour_tri_path_dup <- function(graph_tri, direction = "lhs") {
  if (direction == "rhs"){
    # rhs
    if (sum(graph_tri$dup) != 0 && length(get_dup_edges(graph_tri, direction = "rhs")) != 0) {
      dup <- get_dup_edges(graph_tri, direction = "rhs", sep = ';')
      dup_split <- str_split(dup, " ")
      colour_s <- btvis::colour_tri_path3(graph_tri, edge_selection = 3)
      for (i in seq(ecount(graph_tri))){
        for (j in seq(length(dup_split))){
          edge <- paste(dup_split[[j]][1], dup_split[[j]][2])
          edge_list_i <- get.edgelist(graph_tri)[i,]
          edge_list_i_p <- paste(edge_list_i[1], edge_list_i[2])
          edge_list_i_p_r <- paste(edge_list_i[2], edge_list_i[1])
          if (edge_list_i_p == edge | edge_list_i_p_r == edge){
            colour_s[i] <- dup[j]
          }
        }
      }
    } else {
      colour_s <- colour_tri_path3(graph_tri, edge_selection = 3)
    }
  } else if (direction == "lhs"){
    # lhs
    if (sum(graph_tri$dup) != 0 && length(get_dup_edges(graph_tri, direction = "lhs")) != 0) {
      dup <- get_dup_edges(graph_tri, direction = "lhs", sep = ';')
      dup_split <- str_split(dup, " ")
      colour_s <- btvis::colour_tri_path3(graph_tri, edge_selection = 1)
      for (i in seq(ecount(graph_tri))){
        for (j in seq(length(dup_split))){
          edge <- paste(dup_split[[j]][2], dup_split[[j]][3])
          edge_list_i <- get.edgelist(graph_tri)[i,]
          edge_list_i_p <- paste(edge_list_i[1], edge_list_i[2])
          edge_list_i_p_r <- paste(edge_list_i[2], edge_list_i[1])
          if (edge_list_i_p == edge | edge_list_i_p_r == edge){
            colour_s[i] <- dup[j]
          }
        }
      }
    } else {
      colour_s <- colour_tri_path3(graph_tri, edge_selection = 1)
    }
  } else {
    stop('edge_selection is either "rhs" or "lhs".')
  }
  colour_s
}
