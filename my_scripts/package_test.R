library(btvis)
library(igraph)
library(tidyverse)
library(ggraph)

# small example
df <- tibble(c1 = sample(LETTERS[1:4], 10, replace = TRUE),
             c2 = sample(1:4, 10, replace = TRUE),
             c3 = sample(LETTERS[23:26], 10, replace = TRUE))
# graph_bi <- btvis::bipartite_graph(df, c("c1", "c2"))
# edge_colours <- colour_bi(graph_bi, 2)
# edge_colours <- colour_edge_selection(graph_bi, c("A", "B", "1"))
# graph_bi %>%
#   ggraph::ggraph(layout = "bipartite") +
#   ggraph::geom_node_point() +
#   ggraph::geom_edge_link(aes(colour = edge_colours)) +
#   ggraph::geom_node_text(ggplot2::aes(label = name), hjust = "outward", vjust = "outward") +  coord_flip() + scale_y_reverse() + scale_x_reverse()


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
  dplyr::filter(Plastic == "PE")
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
