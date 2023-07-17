library(btvis)
library(igraph)
library(tidyverse)
library(ggraph)

colour_tri_selection <- function(graph_tri, selection){
  mtx <- graph_tri$mtx
  mtx_p <- paste(mtx[,1], mtx[,2], mtx[,3] )
  str_detect(mtx_p[1], selection)
  ecount(graph_tri)
  colours <- rep("Other", ecount(graph_tri))
  for (i in seq(nrow(mtx))){
    for (j in seq(length(selection))){
      if (str_detect(mtx_p[i], selection[j])){
        colours[igraph::E(graph_tri)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]] <- selection[j]
      }
    }
  }
  colours
}

df <- tibble(c1 = sample(LETTERS[1:3], 7, replace = TRUE),
             c2 = sample(1:4, 7, replace = TRUE),
             c3 = sample(letters[23:26], 7, replace = TRUE))
graph_tri <- btvis::tripartite_graph(df, c("c1", "c2", "c3"))
graph_tri_s <- igraph::simplify(graph_tri)

colours <- colour_tri_selection(graph_tri_s, selection = c("A"))

graph_tri %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) +
  geom_edge_link(aes(colour = colours),
                 ) +
  geom_node_label(aes(label = name),
                 hjust = "outward",
                 vjust = "inward",
                 ) +
  geom_node_point() +
  # theme_void() +
  scale_x_reverse() +
# mtx <- graph_tri$mtx
# mtx_p <- paste(mtx[,1], mtx[,2], mtx[,3] )
# str_detect(mtx_p[1], selection)
# ecount(graph_tri)
# colours <- rep("Other", ecount(graph_tri))
# for (i in seq(nrow(mtx))){
#   for (j in seq(length(selection))){
#     if (str_detect(mtx_p[i], selection[j])){
#       colours[igraph::E(graph_tri)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]] <- selection[j]
#     }
#   }
# }
# colours




# colour_tri_path3 <- function(graph_tri, mtx, edge_selection = 1){
#   mtx <- graph_tri$mtx
#   colours <- vector(length = ecount(graph_tri))
#   for(i in seq(nrow(mtx))){
#     colours[igraph::E(graph_tri)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]] <- mtx[i,edge_selection]
#   }
#   colours
# }



# graph_tri_s <- igraph::simplify(graph_tri)
# direction <- "lhs"
# colour_s <- colour_tri_path_dup(graph_tri, direction = direction)
# colour_s <- colour_tri_path3(graph_tri_s, edge_selection = 2)
# weights <- edge_weights_n(graph_tri)
graph_tri %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) +
  geom_edge_link(aes(colour = colours),
                 ) +
  geom_node_label(aes(label = name),
                 hjust = "outward",
                 vjust = "inward",
                 ) +
  geom_node_point() +
  # theme_void() +
  scale_x_reverse() +
  scale_edge_width_identity()
graph_tri$mtx
get_dup_edges(graph_tri, direction = direction)










# library(hexSticker)
library(showtext)
font_paths()
font_add(family = "ComicCode", regular = "/usr/local/share/fonts/ComicCode-Regular.otf")
showtext_auto()
sticker(p, package = "btvis", filename = "~/btvis/hex.png",p_size=7, s_x=1, s_y=.8, s_width=0.9, s_height=0.9, )


graph_tri$el
graph_tri$mtx

get_dup_edges(graph_tri)
colours <- colour_tri_path3(graph_tri)
graph_tri_s %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) +
  geom_node_point() +
  geom_edge_link(aes(colour = colour_s)) +
  geom_node_text(aes(label = name),
                 hjust = "outward",
                 vjust = "outward")+
  theme_graph(background = "white") +
  scale_x_reverse()


# btvis::colour_tri_path3(graph_tri_s, edge_selection = 3)
# btvis::get_dup_edges(graph_tri, direction = "lhs", sep = ";")
# btvis::get_dup_edges(graph_tri, direction = "rhs", sep = ";")
# get.edgelist(graph_tri_s)
# # rhs
# if (sum(graph_tri$dup) != 0 && length(get_dup_edges(graph_tri)) != 0) {
#   dup <- get_dup_edges(graph_tri, direction = "rhs", sep = ';')
#   dup_split <- str_split(dup, " ")
#   colour_s <- btvis::colour_tri_path3(graph_tri_s, edge_selection = 3)
#   for (i in seq(ecount(graph_tri_s))){
#     for (j in seq(length(dup_split))){
#       edge <- paste(dup_split[[j]][1], dup_split[[j]][2])
#       edge_list_i <- get.edgelist(graph_tri_s)[i,]
#       edge_list_i_p <- paste(edge_list_i[1], edge_list_i[2])
#       edge_list_i_p_r <- paste(edge_list_i[2], edge_list_i[1])
#       if (edge_list_i_p == edge | edge_list_i_p_r == edge){
#         colour_s[i] <- dup[j]
#       }
#     }
#   }
# } else {
#   colour_s <- colour_tri_path3(graph_tri_s)
# }
#
# # lhs
# if (sum(graph_tri$dup) != 0 && length(get_dup_edges(graph_tri)) != 0) {
#   dup <- get_dup_edges(graph_tri, direction = "lhs", sep = ';')
#   dup_split <- str_split(dup, " ")
#   colour_s <- btvis::colour_tri_path3(graph_tri_s, edge_selection = 1)
#   for (i in seq(ecount(graph_tri_s))){
#     for (j in seq(length(dup_split))){
#       edge <- paste(dup_split[[j]][2], dup_split[[j]][3])
#       edge_list_i <- get.edgelist(graph_tri_s)[i,]
#       edge_list_i_p <- paste(edge_list_i[1], edge_list_i[2])
#       edge_list_i_p_r <- paste(edge_list_i[2], edge_list_i[1])
#       if (edge_list_i_p == edge | edge_list_i_p_r == edge){
#         colour_s[i] <- dup[j]
#       }
#     }
#   }
# } else {
#   colour_s <- colour_tri_path3(graph_tri_s)
# }
