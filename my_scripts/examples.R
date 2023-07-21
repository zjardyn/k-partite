library(btvis)
library(igraph)
library(tidyverse)
library(ggraph)

# simple example
df <- tibble(c1 = sample(LETTERS[1:4], 10, replace = TRUE),
             c2 = sample(LETTERS[23:26], 10, replace = TRUE),
             c3 = sample(1:10, 10, replace = TRUE))

# make the graph
graph_bi <- btvis::bipartite_graph(df = df, edge_list = c("c1", "c2"))

# colour based on edge nodes
colours <- colour_bi(graph_bi = graph_bi, edge_nodes = 1)

# colour based on node selection
colours <- colour_edge_selection(graph_bi = graph_bi, nodes_s = c("A", "W"), binary = F)

graph_bi %>%
  ggraph(layout = "bipartite") +
  geom_node_point() +
  geom_edge_link(aes(colour = colours)) +
  geom_node_text(aes(label = name), hjust = "outward", vjust = "outward") +  coord_flip() + scale_y_reverse() + scale_x_reverse()

# real-world example
df2 <- readr::read_rds("/home/zjardyn/btvis/my_scripts/edge_list.rds")

# filter rows based on plastic type and remove spaces from cells and empty entries
df_s <- df2 %>%
  dplyr::filter(Plastic == "PE" | Plastic == "LDPE" | Plastic == "PS") %>%
  mutate(Domain = str_replace_all(Domain, " ", "_")) %>%
  mutate(Enzyme = str_replace_all(Enzyme, " ", "_")) %>%
  mutate(Organism = str_replace_all(Organism, " ", "_")) %>%
  filter(Domain != "-")

graph_bi_p <- btvis::bipartite_graph(df = df_s, edge_list = c("Plastic", "Enzyme"))

colours <- colour_bi(graph_bi_p, edge_nodes = 1)
colours <- colour_edge_selection(graph_bi_p, nodes_s = "[aA]lkane")
selection <- c("[lL]accase", "[iI]ntegral", "membrane")
colours <- colour_edge_selection(graph_bi_p, nodes_s = selection , binary = FALSE)

graph_bi_p %>%
  ggraph(layout = "bipartite") +
  geom_node_point() +
  geom_edge_link(aes(colour = colours),
                 edge_width = 1.5) +
  geom_node_text(aes(label = name), hjust = "outward", vjust = "outward") +
  coord_flip() +
  scale_y_reverse(limits = c(1.1, -0.6)) +
  scale_x_reverse()

# weighted example
weights <- edge_weights_n(graph_bi_p)
graph_bi_p_s <- igraph::simplify(graph_bi_p)
colours <- colour_bi(graph_bi_p_s, 1)

graph_bi_p_s %>%
  ggraph(layout = "bipartite") +
  geom_node_point() +
  geom_edge_link(aes(colour = colours,
                     edge_width = weights),
                 alpha = 0.6) +
  geom_node_text(aes(label = name), hjust = "outward", vjust = "outward") +
  coord_flip() +
  scale_y_reverse(limits = c(1.1, -0.6)) +
  scale_x_reverse()

# Things I am working on:
# Colour by external variable
# Weights by external variable

# tripartite graphs
df <- tibble(c1 = sample(LETTERS[1:4], 10, replace = TRUE),
             c3 = sample(1:4, 10, replace = TRUE),
             c2 = sample(LETTERS[23:26], 10, replace = TRUE))
# make tripartite graph
graph_tri <- btvis::tripartite_graph(df = df, edge_list = c("c1", "c2", "c3"))
# path colours
# colours <- colour_tri_path_dup(graph_tri = graph_tri, direction = "hs")
colours <- colour_tri_path3(graph_tri = graph_tri, edge_selection = 2 )
graph_tri %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) +
  geom_node_point() +
  geom_edge_link(aes(colour = colours,
                     edge_width = NULL)) +
  geom_node_text(aes(label = name),
                 hjust = "outward",
                 vjust = "outward")+
  scale_x_reverse()

# Run this chunk all at once
df <- tibble(c1 = sample(LETTERS[1:4], 10, replace = TRUE),
             c3 = sample(1:4, 10, replace = TRUE),
             c2 = sample(LETTERS[23:26], 10, replace = TRUE))
graph_tri <- btvis::tripartite_graph(df = df, edge_list = c("c1", "c2", "c3"))
graph_tri_s <- igraph::simplify(graph_tri)
direction <- "rhs"
colours <- colour_tri_path_dup_s(graph_tri = graph_tri, direction = direction)
weights <- edge_weights_n(graph_tri)
graph_tri_s %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) +
  geom_node_point() +
  geom_edge_link(aes(colour = colours,
                     edge_width = weights)) +
  geom_node_text(aes(label = name),
                 hjust = "outward",
                 vjust = "outward")+
  scale_x_reverse() +
  scale_edge_width_identity()
get_dup_edges(graph_tri, direction = direction)


# real-world example
df2 <- readr::read_rds("/home/zjardyn/btvis/my_scripts/edge_list.rds")

# filter rows based on plastic type and remove spaces from cells and empty entries
df_s <- df2 %>%
  dplyr::filter(Plastic == "PE" | Plastic == "LDPE" | Plastic == "PS") %>%
  mutate(Domain = str_replace_all(Domain, " ", "_")) %>%
  mutate(Enzyme = str_replace_all(Enzyme, " ", "_")) %>%
  mutate(Organism = str_replace_all(Organism, " ", "_")) %>%
  filter(Domain != "-")

graph_tri_p <- btvis::tripartite_graph(df = df_s, edge_list = c("Organism", "Enzyme", "Domain") )
# colours <- colour_tri_path_dup(graph_tri_p)
colours <- colour_tri_path3(graph_tri_p, edge_selection = 2)
graph_tri_p %>%
  ggraph(layout = graph_tri_p$l$layout[,2:1]) +
  geom_node_point() +
  geom_edge_link(aes(colour = colours),
                 show.legend = FALSE) +
  geom_node_text(aes(label = name),
                 hjust = "outward",
                 vjust = "outward")+
  scale_x_reverse(limits = c(3.3, 0.2))

df_s_2 <- df2 %>%
  mutate(Domain = str_replace_all(Domain, " ", "_")) %>%
  mutate(Plastic = str_replace_all(Plastic, " ", "_")) %>%
  mutate(Enzyme = str_replace_all(Enzyme, " ", "_")) %>%
  mutate(Organism = str_replace_all(Organism, " ", "_")) %>%
  filter(Domain != "-")
graph_tri_p <- btvis::tripartite_graph(df = df_s_2, edge_list = c("Plastic", "Organism", "Enzyme"))
graph_tri_p_s <- igraph::simplify(graph_tri_p)
colours <- colour_tri_path_dup_s(graph_tri = graph_tri_p, direction = "lhs")
weights <- edge_weights_n(graph_tri_p)


graph_tri_p_s %>%
  ggraph(layout = graph_tri_p$l$layout[,2:1]) +
  geom_node_point() +
  geom_edge_link(aes(colour = colours,
                     edge_width = weights),
                 alpha = 0.6,
                 show.legend = FALSE) +
  geom_node_text(aes(label = name),
                 size = 2,
                 hjust = "outward",
                 vjust = "outward")+
  scale_x_reverse(limits = c(3.3, 0.2))




