library(btvis)
library(igraph)
library(tidyverse)
library(ggraph)

# small example
df <- tibble(c1 = sample(LETTERS[1:4], 10, replace = TRUE),
             c2 = sample(LETTERS[23:26], 10, replace = TRUE),
             c3 = sample(1:10, 10, replace = TRUE))




graph_bi <- btvis::bipartite_graph(df, c("c1", "c2"))

igraph::as_adj(graph_bi)



edge_colours <- colour_bi(graph_bi, 2)
# edge_colours <- colour_edge_selection(graph_bi, c("A", "B", "1"))
graph_bi %>%
  ggraph::ggraph(layout = "bipartite") +
  ggraph::geom_node_point() +
  ggraph::geom_edge_link(aes(colour = edge_colours)) +
  ggraph::geom_node_text(ggplot2::aes(label = name), hjust = "outward", vjust = "outward") +  coord_flip() + scale_y_reverse() + scale_x_reverse()


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
