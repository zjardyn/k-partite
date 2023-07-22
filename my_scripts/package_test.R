library(btvis)
library(igraph)
library(tidyverse)
library(ggraph)

# small example
df <- tibble(c1 = sample(LETTERS[1:4], 10, replace = TRUE),
             c2 = sample(LETTERS[23:26], 10, replace = TRUE),
             c3 = sample(1:10, 10, replace = TRUE),
             c4 = c(rep("yes",5), rep("no", 5)))
graph_bi <- btvis::bipartite_graph(df, c("c1", "c2"))
### colour by external variable bipartite ###
el_bi <- get.edgelist(graph_bi) # edgelist of graph_bi
variable <- df$c4 # the variable we want to colour by

## Method 2.
el_bi_p <- paste(el_bi[,1], el_bi[,2]) # edge
el_v <- data.frame(el = el_bi_p, v = variable)
el_v_a <- aggregate(el_v$v, list(el_v$el), FUN=paste, collapse= " " )
v_a_u <- sapply(strsplit(el_v_a$x, " "), trimws) %>%
  sapply(unique) %>%
  sapply(paste, collapse = " ")
el_v_a$x <- v_a_u
el_bi_s <- get.edgelist(igraph::simplify(graph_bi))
el_bi_s_p <- paste(el_bi_s[,1], el_bi_s[,2])
idx <- match(el_bi_s_p, el_v_a$Group.1)
colours <- el_v_a$x[idx]
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
