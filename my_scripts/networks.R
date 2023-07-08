library(tidyverse)
library(igraph)
library(ggraph)
library(RColorBrewer)
library(rlang)
library(glue)

#TODO: Add weight based on numerical variable.
#TODO: See if you can label edges based on the weight.
#TODO: Look into ordering the nodes.
#TODO: For top-down plots, find way to scale y-axis and make node labels vertical.
#TODO: Look at built-in datasets for plotting examples.(Airline destination/arrival)
#TODO: Make a grouped agglomerated variable to use for weights.
#TODO: Write parsing function for iprscan tsv files.
#TODO: See how far you can get with tripartite graphs.
#TODO: See if colouring based on outer edges is possible. Can you get an edge list from a tripartite graph?
#TODO: Colour points by hidden variable: Organism--Enzyme coloured by plastic

rm_prefix_v <- function(graph_bi, prefix) {
  igraph::V(graph_bi)$name <- igraph::V(graph_bi)$name %>%
    stringr::str_replace(prefix, "")
  graph_bi
}

rp_prefix_v <- function(graph_bi, prefix, replacement) {
  igraph::V(graph_bi)$name <- igraph::V(graph_bi)$name %>%
    stringr::str_replace(prefix, replacement)
  graph_bi
}

my_bipartite_graph <- function(edge_list, edge_selection) {
  edge_selection <- rev(edge_selection)
  graph_obj <- edge_list %>%
    dplyr::select(tidyselect::all_of(edge_selection))%>%
    dplyr::rename(e1 := !!edge_selection[1]) %>%
    dplyr::rename(e2 := !!edge_selection[2]) %>%
    dplyr::mutate(e1 = stringr::str_c("2_", e1)) %>%
    dplyr::mutate(e2 = stringr::str_c("1_",e2)) %>%
    igraph::graph.data.frame(directed = FALSE)
  igraph::V(graph_obj)$type <- igraph::bipartite.mapping(graph_obj)$type
  graph_obj
}

colour_edges_bi <- function(graph_bi, edge_number = 1){
  e_idx <- igraph::V(graph_bi)$name %>%
    stringr::str_detect(glue::glue("^{edge_number}_"))
  e_name <- igraph::V(graph_bi)$name[e_idx]
  edge_colours <- vector(mode = "character", length = igraph::ecount(graph_bi))
  for(v in e_name){
    selected <- igraph::E(graph_bi)[.from(v)]
    edge_colours[selected] <- v
  }
  edge_colours_r <- edge_colours %>%
    stringr::str_replace(glue::glue("^{edge_number}_"), "")
  igraph::E(graph_bi)$colours <- edge_colours_r
  graph_bi
}

colour_edges_selection_bi <- function(graph_bi, selection){
  selection_c <- selection %>%
    paste(collapse = "|")
  idx <- str_detect(igraph::V(graph_bi)$name, selection_c)
  selection_names <- igraph::V(graph_bi)$name[idx]
  edge_colours <- rep(FALSE, igraph::ecount(graph_bi))
  for(i in selection_names){
    selected <- igraph::E(graph_bi)[.from(i)]
    edge_colours[selected] <- TRUE
  }
  igraph::E(graph_bi)$colours <- edge_colours
  graph_bi
}

colour_edges_selection_bi_2 <- function(graph_bi, selection){
  selection_c <- selection %>%
    paste(collapse = "|")
  idx <- stringr::str_detect(igraph::V(graph_bi)$name, selection_c)
  selection_names <- igraph::V(graph_bi)$name[idx]
  edge_colours <- rep("Other", igraph::ecount(graph_bi))
  for(i in selection_names){
    selected <- igraph::E(graph_bi)[.from(i)]
    edge_colours[selected] <- i
  }
  igraph::E(graph_bi)$colours <- edge_colours
  graph_bi
}

get_n_edges <- function(graph_bi){
  el <- igraph::get.edgelist(graph_bi)
  el_c <-paste(el[,1], el[,2], sep = "--")
  el_tib <- tibble::tibble(el = el_c)
  edge_width <- el_tib %>%
    dplyr::group_by(el) %>%
    dplyr::summarize(n = n())
  edge_width
}

get_edgelist <- function(graph_bi){
  el <- igraph::get.edgelist(graph_bi)
  el_c <-paste(el[,1], el[,2], sep = "--")
  el_tib <- tibble::tibble(el = el_c)
  el_tib
}

edge_weights_n <- function(graph_bi){
  el <- igraph::get.edgelist(graph_bi)
  el_df <- tibble::tibble(el = paste(el[,1], el[,2], sep = "--")) %>%
    dplyr::group_by(el) %>%
    dplyr::summarize(n = n())
  el_s <- igraph::get.edgelist(igraph::simplify(graph_bi))
  el_v <- paste(el_s[,1], el_s[,2], sep = "--")
  el_df[match(el_v, el_df$el),]
}

split_node_label_newline <- function(graph_bi, n = 25){
  graph_bi_n <- igraph::V(graph_bi)$name
  dplyr::if_else(nchar(graph_bi_n) > n,
    paste(substring(graph_bi_n, 0, n), 
          substring(graph_bi_n, n + 1), 
          sep = "\n"),
    graph_bi_n)
}

offset_node_label <- function(graph_bi){
  graph_bi_n <- igraph::V(graph_pe_s_c)$name
  condition <- stringr::str_detect(graph_bi_n, "^1")
  my_true <- paste(graph_bi_n, "  ")
  my_false <- paste("  ", graph_bi_n)
  dplyr::if_else(condition, my_true, my_false)
}

offset_node_label_newline <- function(graph_bi) {
  strings <- igraph::V(graph_bi)$name 
  formatted_strings <- tibble::tibble(strings) %>%
    dplyr::mutate(formatted = dplyr::case_when(
      stringr::str_detect(strings, "^1") ~ stringr::str_replace(strings, "(.*)$", "\\1  "),
      stringr::str_detect(strings, "^2") ~ stringr::str_replace(strings, "^(.*)", "  \\1"),
      TRUE ~ strings
    )) %>%
    dplyr::mutate(formatted = dplyr::case_when(
      stringr::str_detect(strings, "^1") ~ stringr::str_replace(formatted, "\n", "  \n"),
      stringr::str_detect(strings, "^2") ~ stringr::str_replace(formatted, "\n", "\n  "),
      TRUE ~ strings
    )) %>%
    dplyr::pull(formatted)
  formatted_strings
}

# Examples
el <- readr::read_rds("/home/zjardyn/OneDrive/pFAMWork/edge_list.rds")

#el %>%
#  dplyr::group_by(Plastic) %>%
#  dplyr::summarise(n = n()) %>%
#  dplyr::arrange(desc(n)) %>%
#  print(n = 49)

el_PE <- el %>%
  dplyr::filter(Plastic == "PE" | Plastic == "LDPE" | Plastic == "PS")
# el_PE <- el %>%
  # filter(Plastic == "PHA" | Plastic == "PHB")
graph_pe <- my_bipartite_graph(el_PE, edge_selection = c("Organism", "Enzyme"))
graph_pe_s <- simplify(graph_pe)
graph_pe_s_c <- colour_edges_bi(graph_pe_s, edge_number = 2)
E(graph_pe_s_c)$weight <- edge_weights_n(graph_pe)$n
V(graph_pe_s_c)$name <- split_node_label_newline(graph_pe_s_c, n = 23)
V(graph_pe_s_c)$name <- offset_node_label_newline(graph_pe_s_c)
graph_pe_s_c %>%
  rp_prefix_v("^1_", "") %>%
  rp_prefix_v("^  2_", "  " ) %>%
  ggraph(layout = "bipartite") +
    geom_node_point(cex = 0.5) +
    geom_edge_link(aes(colour = colours,
                       edge_width = weight),
                   lineend = "round") +
                   # show.legend = FALSE)+
    geom_node_text(aes(label = name),
                   hjust = "outward",
                   size = 3) + 
    theme_graph(background = "white") +
    coord_flip() +
    # scale_y_reverse()
    # scale_x_reverse() +
    scale_y_binned()

graph_plastic_2 <- my_bipartite_graph(edge_list = el, edge_selection = c("Plastic", "Organism"))
plastics <- c("PET", "PLA")
graph_plastic_2_coloured <- colour_edges_selection_bi_2(graph_plastic_2, selection = plastics)

# selection <- c("PET")
# selection_c <- selection %>%
#   paste(collapse = "|")
# # e_name <- V(graph_bi)$name
# idx <- str_detect(V(graph_plastic_2)$name, selection_c)
# selection_names <- V(graph_plastic_2)$name[idx]

# edge_colours <- rep(FALSE, ecount(graph_plastic_2))

# for(i in selection_names){
#   selected <- E(graph_plastic_2)[.from(i)]
#   edge_colours[selected] <- TRUE
# }
# E(graph_plastic_2)$colours <- edge_colours

graph_plastic_2_coloured %>%
  ggraph(layout = "bipartite") +
    geom_node_point(cex = 0.5, alpha = 0.7) + 
    geom_edge_link(aes(colour = E(graph_plastic_2_coloured)$colours),
                   show.legend = FALSE,
                   width = 2)+
    geom_node_text(aes(label = name),
                   hjust = "outward",
                   size = 3) + 
    theme_graph(background = "white") +
    coord_flip() +
    scale_y_binned()


per_plastic_org_graph <- el %>%
  filter(Plastic == "PET") %>%
  select(all_of(edge_s))%>%
  rename(e1 := !!edge_s[1]) %>%
  rename(e2 := !!edge_s[2]) %>%
  mutate(e1 = str_c("1_", e1)) %>%
  mutate(e2 = str_c("2_",e2)) %>%
  graph.data.frame(directed = FALSE)

V(per_plastic_org_graph)$type <- bipartite.mapping(per_plastic_org_graph)$type

e1_idx <- V(per_plastic_org_graph)$name %>%
  str_detect("^1_")
e1_name <- V(per_plastic_org_graph)$name[e1_idx]

edge_colours <- vector(mode = "character", length = ecount(per_plastic_org_graph))
for(v in e1_name){
  selected <- E(per_plastic_org_graph)[.from(v)]
  edge_colours[selected] <- v
}

edge_colours_r <- edge_colours %>%
  str_replace("^1_", "")

E(per_plastic_org_graph)$colours <- edge_colours_r

per_plastic_org_graph %>%
  rm_prefix_v("^1_") %>%
  rm_prefix_v("^2_") %>%
  ggraph(layout = "bipartite") + 
    geom_node_point(cex = 0.5, alpha = 0.7) + 
    geom_edge_link(aes(colour = edge_colours_r))+
    geom_node_text(aes(label = name),
                   hjust = "outward",
                   size = 3) + 
    theme_graph(background = "white") +
    coord_flip() + 
    scale_y_binned() + 
    theme(legend.position = "bottom")
