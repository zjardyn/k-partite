#' Get duplicated edges from either end of a tripartite graph, displaying the multiple edges.
#'
#' @param graph_tri A tripartite graph object.
#' @param direction A string of either "lhs" or "rhs" Chooses what side of graph to return duplicates.
#' @returns A character vector containing the duplicates, used to set unique colours to duplicated edges.
#' @examples
#' library(tibble)
#' library(igraph)
#' el <- tibble(c1 = c("A", "A", "B", "B", "C", "C", "D"),c2 = c(1, 2, 2, 3, 3, 4, 4))
#' graph_bi <- my_bipartite_graph(el, c("c1", "c2"))
#' E(graph_bi)$colour <- colour_edges_bi(graph_bi, 1)
#' @export
get_dup_edges <- function(graph_tri, direction = "lhs"){
  el <- graph_tri$el
  mtx <- graph_tri$mtx
  dup <- graph_tri$dup
  if (sum(dup) != 0) {
    # get duplicates edges and paste into | syntax
    selection <- unique(paste(el[dup,][,1], el[dup,][,2])) %>%
      paste(collapse = "|")
    # paste mtx into string vector on the rows
    mtxp <- paste(mtx[,1], mtx[,2], mtx[,3])
    # obtain duplicate edge paths from mtxp
    elements <- tibble(Element = mtxp[str_detect(mtxp, selection)]) %>%
      separate(Element, into = c("c1", "c2", "c3"), sep = " ")
    if (direction == "lhs"){
      elements1 <- elements %>%
        unite(c23, c2, c3, sep = " ") %>%
        distinct()
      elements2 <- elements1 %>%
        group_by(c23) %>%
        summarise(c1 = paste(c1, collapse = "|"))
      elements3 <- elements2 %>%
        filter(str_detect(c1, "\\|"))
      unique_chars <- map_chr(elements3$c1, ~ strsplit(.x, "|") %>% unlist() %>% unique() %>% paste(collapse = ""))
      elements3$c1 <- unique_chars
      elements4 <- paste(elements3$c1, elements3$c23)
      elements4 %>%
        str_replace("\\|", "")
    } else if (direction == "rhs"){
      elements1 <- elements %>%
        unite(c12, c1, c2, sep = " ") %>%
        distinct()
      elements2 <- elements1 %>%
        group_by(c12) %>%
        summarise(c3 = paste(c3, collapse = "|"))
      elements3 <- elements2 %>%
        filter(str_detect(c1, "\\|"))
      unique_chars <- map_chr(elements3$c3, ~ strsplit(.x, "|") %>% unlist() %>% unique() %>% paste(collapse = ""))
      elements3$c3 <- unique_chars
      elements4 <- paste(elements3$c12, elements3$c3)
      elements4 %>%
        str_replace("\\|", "")
    } else {
      stop('Direction is either "lhs" or "rhs".')
    }
  } else {
    return(vector())
  }
}
