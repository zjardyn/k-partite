library(btvis)
library(igraph)
library(tidyverse)
library(ggraph)

# small example
df <- tibble(c1 = c("A", "A", "B", "B", "B", "C", "C", "D"), c2 = c(1, 2, 1, 2, 3, 3, 4, 4))
graph_bi <- btvis::bipartite_graph(df, c("c1", "c2"))
E(graph_bi)$colour <- colour_edge_selection(graph_bi, c("A", "4"), binary = TRUE)
# V(graph_bi)$name <- offset_node_label(graph_bi, 1, offset = "\n")
ggbipart(graph_bi, edge_colour = E(graph_bi)$colour, horizontal = FALSE)


rm(list=ls())
devtools::document()
devtools::install()


offset_node_label_newline <- function(graph_bi, n = 2, offset = " "){
  nodes <- igraph::V(graph_bi)$name
  el <- igraph::get.edgelist(graph_bi)
  lhs <- el[,1] %>%
    as.vector() %>%
    unique() %>%
    paste(collapse = "|")
  rhs <- el[,2] %>%
    as.vector() %>%
    unique() %>%
    paste(collapse = "|")
tibble::tibble(nodes) %>%
  dplyr::mutate(formatted = dplyr::case_when(
    stringr::str_detect(nodes, lhs) ~ paste(nodes, strrep(offset, n)),
    stringr::str_detect(nodes, rhs) ~ paste(strrep(offset, n), nodes),
    TRUE ~ nodes)) %>%
  dplyr::mutate(formatted= dplyr::case_when(
    stringr::str_detect(nodes, lhs) ~ stringr::str_replace(formatted, "\n", paste("\n", strrep(offset, n))),
    stringr::str_detect(nodes, rhs) ~ stringr::str_replace(formatted, "\n", paste(strrep(offset, n), "\n")),
    TRUE ~ nodes)) %>%
  dplyr::pull(formatted)
}




## big example
el <- readr::read_rds("/home/zjardyn/OneDrive/pFAMWork/edge_list.rds")
el_PE <- el %>%
  dplyr::filter(Plastic == "PE" | Plastic == "LDPE" | Plastic == "PS")
graph_pe <- btvis::bipartite_graph(el_PE, edge_list = c("Organism", "Enzyme"))
graph_pe_s <- igraph::simplify(graph_pe)
E(graph_pe_s)$weight <- edge_weights_n(graph_pe)
E(graph_pe_s)$colour <- colour_edge_selection(graph_pe_s, "[aA]lkane") 
V(graph_pe_s)$name <-  split_node_label_newline_n(graph_pe_s)
V(graph_pe_s)$name <- offset_node_label_newline(graph_pe_s)


# ggbipart(graph_pe_s, edge_colour = NULL, edge_weight = E(graph_pe_s)$weight, horizontal = TRUE)
ggbipart(graph_pe_s, edge_colour = E(graph_pe_s)$colour, horizontal = TRUE)
