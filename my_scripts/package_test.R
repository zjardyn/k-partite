library(rpartite)
library(igraph)
library(tidyverse)
library(ggraph)

colour_bi_var_d <- function(graph_bi, df, var){
  el_bi <- get.edgelist(graph_bi)
  selection <- df %>%
    pull({{ var }}) # variable to colour by
  el_bi_p <- paste(el_bi[,1], el_bi[,2]) # edges pasted together
  el_v <- data.frame(el = el_bi_p, v = selection)
  # aggregate duplicate edges by pasting together the colouring variable
  el_v_a <- aggregate(el_v$v, list(el_v$el), FUN=paste, collapse= " " )
  # remove duplicate entries of pasted colouring variable
  v_a_u <- sapply(strsplit(el_v_a$x, " "), trimws) %>%
    sapply(unique) %>%
    sapply(paste, collapse = " ")
  el_v_a$x <- v_a_u
  # remove duplicates from edgelist, paste together and match from aggregated edges
  el_bi_s <- get.edgelist(igraph::simplify(graph_bi))
  el_bi_s_p <- paste(el_bi_s[,1], el_bi_s[,2])
  # get the indexes of matched edges
  idx <- match(el_bi_s_p, el_v_a$Group.1)
  colours <- el_v_a$x[idx]
  colours
}

k_partite_graph <- function(df, edge_list){
  mtx <- df %>%
    dplyr::select(tidyselect::all_of(edge_list))%>%
    as.matrix()
  el <- matrix(nrow = 0, ncol = 2)
  for (c in seq(ncol(mtx) - 1)){
    el <- rbind(el, mtx[, c:(c + 1)])
  }
  graph_k <- igraph::graph_from_edgelist(el, directed = FALSE)
  layers <- ceiling(match(V(graph_k)$name, mtx)/nrow(mtx))
  l <- igraph::layout_with_sugiyama(graph_k, layers)
  graph_k$mtx <- mtx
  graph_k$el <- el
  graph_k$l <- l
  graph_k$dup <- duplicated(graph_k$el) | duplicated(graph_k$el,fromLast = TRUE )
  graph_k
}

# small example
df <- tibble(c1 = sample(LETTERS[1:4], 15, replace = TRUE),
             c2 = sample(LETTERS[23:26], 15, replace = TRUE),
             c3 = sample(1:5, 15, replace = TRUE),
             c4 = c(rep("yes",5), rep("no", 5), rep("maybe", 5)),
             c5 = rnorm(15))
graph_k <- k_partite_graph(df, edge_list = c("c1", "c2", "c3", "c4", "c5"))
mtx <- graph_k$mtx
el <- graph_k$el
colours <- rep(mtx[,3], ncol(mtx) - 1)
lims <- graph_k$l$layout[,2]
param <- 0.3
graph_k %>%
  ggraph::ggraph(layout = graph_k$l$layout) +
  ggraph::geom_node_point() +
  ggraph::geom_edge_fan(aes(colour = colours)) +
  ggraph::geom_node_text(ggplot2::aes(label = name), hjust = "outward", vjust = "outward") +
  coord_flip() + scale_y_reverse(limits = c(max(lims) + param, min(lims) - param)) + scale_x_reverse()





graph_bi <- btvis::bipartite_graph(df, c("c2", "c2"))
colours <- colour_bi_var_d(graph_bi = graph_bi, df = df, var = c4)
graph_bi_s <- igraph::simplify(graph_bi)
weights <- edge_weights_n(graph_bi)
graph_bi_s %>%
  ggraph::ggraph(layout = "bipartite") +
  ggraph::geom_node_point() +
  ggraph::geom_edge_link(aes(colour = colours,
                             edge_width = weights)) +
  ggraph::geom_node_text(ggplot2::aes(label = name), hjust = "outward", vjust = "outward") +  coord_flip() + scale_y_reverse() + scale_x_reverse()
df
E(graph_bi_s)
as.data.frame(colours)

el_bi
el_bi_s
graph_tri <- btvis::tripartite_graph(df, c("c1", "c2", "c3"))

rm(list=ls())
devtools::document()
devtools::install()

idx_d <- duplicated(el_bi_p) | duplicated(el_bi_p,fromLast = TRUE )
library(emphatic)
df %>%
  hl('red', rows = idx_d)


## big example
df <- readr::read_rds("/home/zjardyn/btvis/my_scripts/edge_list.rds")
df %>%
  group_by(Plastic) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 40)
df_s <- df %>%
  dplyr::filter(Plastic == "PE" | Plastic == "LDPE" | Plastic == "PS") %>%
  mutate(Domain = str_replace_all(Domain, " ", "_")) %>%
  mutate(Enzyme = str_replace_all(Enzyme, " ", "_")) %>%
  mutate(Organism = str_replace_all(Organism, " ", "_")) %>%
  filter(Domain != "-")
graph_bi_p <- btvis::bipartite_graph(df_s, c("Domain", "Enzyme"))
colours <- colour_bi_var_d(graph_bi_p, df_s, Organism)
graph_bi_ps <- igraph::simplify(graph_bi_p)
weights <- edge_weights_n(graph_bi_p)
graph_bi_ps %>%
  ggraph::ggraph(layout = "bipartite") +
  ggraph::geom_node_point() +
  ggraph::geom_edge_link(aes(colour = colours,
                             edge_width = weights)) +
  ggraph::geom_node_text(ggplot2::aes(label = name), hjust = "outward", vjust = "outward") +  coord_flip() + scale_y_reverse() + scale_x_reverse()




graph_tri <- tripartite_graph(df_s, c("Plastic","Organism", "Enzyme"))
graph_tri_s <- igraph::simplify(graph_tri)
direction <- "lhs"
colour_s <- colour_tri_path_dup(graph_tri, direction = direction)
# colour_s <- colour_tri_path3(graph_tri_s, edge_selection = 2)
weights <- edge_weights_n(graph_tri)
graph_tri_s %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) +
  geom_node_point() +
  geom_edge_link(aes(colour = colour_s, edge_width = weights),
                 show.legend = F,
                 alpha = 0.6) +
  geom_node_text(aes(label = name),
                 hjust = "outward",
                 vjust = "outward")+
  theme_graph(background = "white") +
  scale_x_reverse(limits = c(3.3, 0.3)) + theme_bw()


get_dup_edges(graph_tri)


graph_bi <- btvis::bipartite_graph(df, c("Organism", "Enzyme"))
edge_colours <- colour_bi(graph_bi, 2)
graph_bi %>%
  ggraph::ggraph(layout = "bipartite") +
  ggraph::geom_node_point() +
  ggraph::geom_edge_link(aes(colour = edge_colours),
                         show.legend = FALSE) +
  ggraph::geom_node_text(ggplot2::aes(label = name), hjust = "outward", vjust = "outward") +
  coord_flip() +
  scale_y_reverse(limits = c(1.4, -0.4)) +
  scale_x_reverse() +
  theme_bw()

# colours <- vector(length = ecount(graph_k))
# for (i in seq(nrow(mtx))){
#   idx <- E(graph_k)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]
#   colours[idx] <- mtx[i,1]
# }
