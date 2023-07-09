library(btvis)
library(igraph)
library(tidyverse)
library(ggraph)

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

df <- tibble(c1 = sample(LETTERS[1:3], 7, replace = TRUE),
             c2 = sample(1:4, 7, replace = TRUE),
             c3 = sample(letters[23:26], 7, replace = TRUE))
graph_tri <- btvis::tripartite_graph(df, c("c1", "c2", "c3"))
graph_tri_s <- igraph::simplify(graph_tri)
if (sum(graph_tri$dup) != 0 && length(get_dup_edges(graph_tri)) != 0) {
  dup <- get_dup_edges(graph_tri, direction = "lhs")
  dup_split <- str_split(dup, " ")
  colour_s <- colour_tri_path3(graph_tri_s)
  for (i in seq(ecount(graph_tri_s))){
    for (j in seq(length(dup_split))){
      edge <- paste(dup_split[[j]][2], dup_split[[j]][3])
      edge_list_i <- get.edgelist(graph_tri_s)[i,]
      edge_list_i_p <- paste(edge_list_i[1], edge_list_i[2])
      if (edge_list_i_p == edge){
        colour_s[i] <- dup[j]
      }
    }
  }
} else {
  colour_s <- colour_tri_path3(graph_tri_s)
}
weights <- edge_weights_n(graph_tri)
graph_tri_s %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) +
  geom_node_point() +
  geom_edge_link(aes(colour = colour_s, edge_width = weights)) +
  geom_node_text(aes(label = name),
                 hjust = "outward",
                 vjust = "outward")+
  theme_graph(background = "white") +
  scale_x_reverse()

graph_tri$el
graph_tri$mtx

get_dup_edges(graph_tri)
colours <- colour_tri_path3(graph_tri)
graph_tri %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) +
  geom_node_point() +
  geom_edge_link(aes(colour = colours)) +
  geom_node_text(aes(label = name),
                 hjust = "outward",
                 vjust = "outward")+
  theme_graph(background = "white") +
  scale_x_reverse()


