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
colour_tri_path <- function(graph_tri, edge_column){
  el <- get.edgelist(graph_tri)
  c1 <- unique(edge_column)
  colours <- vector(length = ecount(graph_tri))
  elp <- paste(el[, 1], el[, 2])
  for (i in c1) {
    for (j in seq(nrow(el)/2)) {
      if (str_detect(elp[j], i)) {
        colours[j] <- i
        colours[j + nrow(el)/2] <- i
      }
    }
  }
  colours
}

#' @export
colour_tri_path2 <- function(graph_tri, el, mtx, dup){
  mtx <- graph_tri$mtx
  dup <- graph_tri$dup
  el <- graph_tri$el
  colours <- vector(length = ecount(graph_tri))
  for(i in seq(nrow(mtx))){
     colours[igraph::E(graph_tri)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]] <- mtx[i,1]
   }
   select <- unique(mtx[,1]) %>%
     paste(collapse = "|")
   if(sum(dup != 0)){
     for(i in seq(length(E(graph_tri)[dup]))){
       if(str_detect(graph_tri$el[dup,][i,1], select) == TRUE){
        colours[E(graph_tri)[dup][i]] <- graph_tri$el[dup,][i,1]
       } else{
        colours[E(graph_tri)[dup][i]] <- paste(graph_tri$el[dup,][i,1], graph_tri$el[dup,][i,2])
       }
     }
   }
   if(sum(dup != 0)){
     vector1<- get_dup_edges(graph_tri$el, graph_tri$mtx, dup)
     vector2 <- colours
     matches <- tibble(Full = vector1) %>%
       mutate(Short = str_extract(Full, "\\s(.*)")) %>%
       mutate(Short = str_trim(Short, side = "left"))
     for(i in seq(nrow(matches))){
       idx <- which(vector2 == matches$Short[i])
       vector2[idx] <- matches$Full[i]
     }
     colours <- vector2
   }
  colours
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

