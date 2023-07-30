library(rpartite)
library(igraph)
library(tidyverse)
library(ggraph)
library(glue)

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

# settings
# 15 rows;1.8
# 25 rows; 1.6
# 50 rows; 1.2



df <- tibble(c1 = sample(LETTERS[1:4], 15, replace = TRUE),
             c2 = sample(LETTERS[23:26], 15, replace = TRUE),
             c3 = sample(1:2, 15, replace = TRUE),
             c4 = c(rep("yes",10), rep("no", 5)),
             c5 = rnorm(15))
# small example
# df <- tibble(c1 = sample(LETTERS[1:4], 1000, replace = TRUE),
#              c2 = sample(LETTERS[23:26], 1000, replace = TRUE),
#              c3 = sample(1:5, 1000, replace = TRUE),
#              c4 = c(rep("yes",900), rep("no", 100)),
#              c5 = round(rnorm(1000), digits = 1))
selection = c("c1", "c2", "c3")
graph_k <- k_partite_graph(df, edge_list = selection)
colours <- rep(graph_k$mtx[,1], ncol(graph_k$mtx) - 1)
lims <- graph_k$l$layout[,2]
# param <- 0.3
label <- paste(graph_k$el[,1], graph_k$el[,2])
param2 <- 1.5
m <- tibble(c = character(), n = numeric())
for (i in seq(selection)[2:length(selection)]) {
  # print(v1)
  m1 <- df %>%
    select((all_of(selection))) %>%
    group_by(!!sym(selection[ i - 1 ]) , !!sym(selection[ i ])) %>% count() %>%
    unite("e", !!sym(selection[ i - 1 ]):!!sym(selection[ i]))
    # filter(n == max(n))
  m <- rbind(m , m1)
  # m %>%
    # filter(n == max(n)) %>% print()
}
m
elp <- paste(graph_k$el[,1],graph_k$el[,2], sep = "_")
weight_idx <- match(elp, m$e)
weights <- vector(length = ecount(graph_k))
for (i in seq(weights)){
  weights[i] <- m[weight_idx[i],2]
}
weights <- unlist(weights)
graph_k %>%
  ggraph::ggraph(layout = graph_k$l$layout) +
  ggraph::geom_node_point()+
  ggraph::geom_edge_fan(aes(colour = colours, label = weights),
                             check_overlap = TRUE,
                             # sep = unit(param2, "mm"),
                             edge_width = param2,
                             lineend = "round",
                             alpha = 0.5,
                             strength = 0.1) +
  ggraph::geom_node_text(ggplot2::aes(label = name), hjust = "outward", vjust = "outward") +
  coord_flip() +
  scale_y_reverse() +
  # scale_y_reverse(limits = c(max(lims)+param, min(lims)-param)) +
  scale_x_reverse()


df <- readr::read_rds("/home/zjardyn/rpartite/my_scripts/edge_list.rds")
# df %>%
#   group_by(Plastic) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   print(n = 40)
df_s <- df %>%
  dplyr::filter(Plastic == "PE" | Plastic == "LDPE" | Plastic == "PS") %>%
  mutate(Domain = str_replace_all(Domain, " ", "_")) %>%
  mutate(Enzyme = str_replace_all(Enzyme, " ", "_")) %>%
  mutate(Organism = str_replace_all(Organism, " ", "_")) %>%
  filter(Domain != "-")
selection <- c("Plastic", "Organism", "Enzyme", "Domain")
graph_k1 <- k_partite_graph(df_s, selection)
colours <- rep(graph_k1$mtx[,3], ncol(graph_k1$mtx) - 1 )
m <- tibble(c = character(), n = numeric())
for (i in seq(selection)[2:length(selection)]) {
  # print(v1)
  m1 <- df_s %>%
    select((all_of(selection))) %>%
    group_by(!!sym(selection[ i - 1 ]) , !!sym(selection[ i ])) %>% count() %>%
    unite("e", !!sym(selection[ i - 1 ]):!!sym(selection[ i]))
  # filter(n == max(n))
  m <- rbind(m , m1)
  # m %>%
  # filter(n == max(n)) %>% print()
}
elp <- paste(graph_k1$el[,1],graph_k1$el[,2], sep = "_")
weight_idx <- match(elp, m$e)
weights <- vector(length = ecount(graph_k1))
for (i in seq(weights)){
  weights[i] <- m[weight_idx[i],2]
}
weights <- unlist(weights)
graph_k1 %>%
  ggraph::ggraph(layout = graph_k1$l$layout[,2:1]) +
  ggraph::geom_node_point() +
  ggraph::geom_edge_fan(aes(colour = colours,
                            label = weights),
                             # sep = unit(0.1, "mm"),
                             check_overlap = TRUE,
                             strength = 0.6,
                             angle_calc = "across") +
  ggraph::geom_node_text(ggplot2::aes(label = name), hjust = "outward", vjust = "outward") +
  coord_flip() +
  scale_y_reverse()
  # scale_y_reverse(limits = c(max(lims) + param, min(lims) - param)) +
  # scale_x_reverse()



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
