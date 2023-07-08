library(btvis)
library(igraph)
library(tidyverse)
library(ggraph)
library(viridis)

df <- tibble(c1 = sample(LETTERS[1:4], 7, replace = TRUE),
             c2 = sample(1:4, 7, replace = TRUE), 
             c3 = sample(LETTERS[23:26], 7, replace = TRUE))
# graph_tri <- tripartite_graph(df, c("c1", "c2", "c3"))
df <- readr::read_rds("/home/zjardyn/OneDrive/pFAMWork/edge_list.rds")
df_pe <- df  %>%
  dplyr::filter(Plastic == "PLA")
graph_tri <- tripartite_graph(df_pe, c("Organism", "Enzyme", "Domain"))
# colours <- colour_tri_path2(graph_tri, graph_tri$el, graph$mtx, graph$dup)
# weights <- edge_weights_n(graph_tri)
# graph_tri_s <- igraph::simplify(graph_tri)
# colours <- colour_tri(graph_tri)
# colours <- colour_tri_path(graph_tri, edge_column = df_pe$Domain)
# weights <- edge_weights_n(graph_tri)
# graph_tri_s <- igraph::simplify(graph_tri)
# colours <- colour_tri(graph_tri)
 
graph_tri_s <- igraph::simplify(graph_tri)
# colours <- colour_tri_path(graph_tri_s, edge_column = df_pe$Plastic)
mtx <- graph_tri_s$mtx
dup <- graph_tri_s$dup
el <- graph_tri_s$el
colours <- vector(length = ecount(graph_tri_s))
for(i in seq(nrow(mtx))){
   colours[igraph::E(graph_tri_s)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]] <- mtx[i,1]
}

weights <- edge_weights_n(graph_tri)
# colours <- colour_tri_path(graph_tri, edge_column = df$c1)
graph_tri_s %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) + 
    geom_node_point() +  
    geom_edge_link(aes(colour = colours, edge_width = weights ),
                   show.legend = F,
                   alpha = 0.7) +
      geom_node_text(aes(label = name),
                     hjust = "outward",
                     vjust = "outward")+ 
  scale_edge_color_viridis(discrete = TRUE, option = "D") + 
  # scale_x_reverse() +
  # theme_graph(background = "white") +
  scale_x_reverse(limits = c(3.3,0))
  theme_minimal()

get_dup_edges(graph_tri$el, graph_tri$mtx, graph_tri$dup)
graph_tri$mtx


vector1<- get_dup_edges(graph_tri$el, graph_tri$mtx, dup)
vector2 <- colours

matches <- tibble(Full = vector1) %>%
  mutate(Short = str_extract(Full, "\\s(.*)")) %>%
  mutate(Short = str_trim(Short, side = "left"))

for(i in seq(nrow(matches))){
  idx <- which(vector2 == matches$Short[i])
  vector2[idx] <- matches$Full[i]
}



str_detect(dup_e, colours)

E(graph_tri)[dup]
graph_tri$el[dup,]


el <- graph_tri$el
mtx <- graph_tri$mtx
selection <- unique(paste(el[dup,][,1], el[dup,][,2])) %>%
      paste(collapse = "|")
    mtxp <- paste(mtx[,1], mtx[,2], mtx[,3])
    elements <- tibble(Element = mtxp[str_detect(mtxp, selection)]) %>%
      separate(Element, into = c("c1", "c2", "c3"), sep = " ") %>%
      unite(c23, c2, c3, sep = " ") %>%
      aggregate(c1~., FUN=paste, collapse="") %>%
      tibble()
    unique_chars <- map_chr(elements$c1, ~ strsplit(.x, "") %>% unlist() %>% unique() %>% paste(collapse = "")) 
    elements$c1 <- unique_chars
    elements <- paste(elements$c1, elements$c23)
    elements
  
get_dup_edges <- function(el, mtx, dup) {
  if(sum(dup)==0){
    print("No duplicates.")
  } else{
    selection <- unique(paste(el[dup,][,1], el[dup,][,2])) %>%
      paste(collapse = "|")
    mtxp <- paste(mtx[,1], mtx[,2], mtx[,3])
    elements <- tibble(Element = mtxp[str_detect(mtxp, selection)]) %>%
      separate(Element, into = c("c1", "c2", "c3"), sep = " ") %>%
      unite(c23, c2, c3, sep = " ") %>%
      aggregate(c1~., FUN=paste, collapse="")
    unique_chars <- map_chr(elements$c1, ~ strsplit(.x, "") %>% unlist() %>% unique() %>% paste(collapse = "")) 
    elements$c1 <- unique_chars
    elements <- paste(elements$c1, elements$c23)
    elements
  } 
}


if(sum(dup)==0){
  print("No duplicates.")
} else{
  selection <- unique(paste(el[dup,][,1], el[dup,][,2])) %>%
    paste(collapse = "|")
  mtxp <- paste(mtx[,1], mtx[,2], mtx[,3])
  elements <- tibble(Element = mtxp[str_detect(mtxp, selection)]) %>%
    separate(Element, into = c("c1", "c2", "c3"), sep = " ") %>%
    unite(c23, c2, c3, sep = " ") %>%
    aggregate(c1~., FUN=paste, collapse="")
  unique_chars <- map_chr(elements$c1, ~ strsplit(.x, "") %>% unlist() %>% unique() %>% paste(collapse = "")) 
  elements$c1 <- unique_chars
  elements <- paste(elements$c1, elements$c23)
  elements
} 






E(graph_tri)[.inc("A|B")]


for(i in seq(length(E(graph_tri)[dup]))){
 colours[E(graph_tri)[dup][i]] <- paste(graph_tri$el[dup,][i,1], graph_tri$el[dup,][i,2]) 
}
# E(igraph::simplify(graph_tri))
# t(graph_tri$el)
unique(mtx[,1])

edge_list = c("c1", "c2", "c3") 
mtx <- df %>%
   dplyr::select(tidyselect::all_of(edge_list))%>%
   as.matrix()
 el <- rbind(mtx[,1:2], mtx[,2:3])
 graph_tri <- graph_from_edgelist(el, directed = FALSE)
 layers <- ceiling(match(V(graph_tri)$name, mtx)/nrow(mtx))
 l <- layout_with_sugiyama(graph_tri, layers)
 graph_tri$mtx <- mtx 
 graph_tri$el <- el
 graph_tri$l <- l 
 graph_tri

 
 # different 
get.edgelist(graph_tri) 
get.edgelist(igraph::simplify(graph_tri))
# same 
V(graph_tri)$name
V(igraph::simplify(graph_tri))$name

match(V(graph_tri)$name, mtx)

el_p <- paste(el[,1], el[,2], sep = "-")
el_p[-duplicated(el_p)]
el_t <- read.table(text=el_p[-duplicated(el_p)],sep='-') ## see @flodel comment
idx <- str_detect(el_t$V1, "[A-Z]")
c1_s <- el_t$V1[idx]


graph_tri_s <- igraph::simplify(graph_tri)
# layers <- ceiling(match(V(graph_tri)$name, mtx)/nrow(mtx))
# graph_tri_s$l <- layout_with_sugiyama(graph_tri, layers)





## WEIGHTS
edge_weights_n(graph_tri)





# overlapping_colors <- make.unique(elp[overlapping], sep = "_")

# colours[overlapping] <- overlapping_colors

overlapping
ol <- unique(elp[overlapping]) %>%
  paste(collapse = "|")
ol
mx <- graph_tri$mtx
mxp <- paste(mx[,1], mx[,2], mx[,3])
mxp
strings <- mxp[str_detect(mxp, ol)]
vectors <- data.frame(Vector = strings)

# Separate the elements, values, and letters using tidyr::separate
vectors <- vectors %>%
  separate(Vector, into = c("Element", "Value", "Letter"), sep = " ")

vectors <- vectors %>%
  unite(New, Value, Letter, sep = "")

vector <- aggregate(data = vectors, Element~., FUN=paste, collapse="")
vector <- paste(vector$Element, vector$New)
vector
colours[overlapping]
colours
elp[overlapping]

graph_tri %>%
  ggraph(layout = graph_tri$l$layout[,2:1]) + 
    geom_node_point() +  
    geom_edge_link(aes(colour = colours),
                   alpha = 0.5,
                   edge_width = 2) +
      geom_node_text(aes(label = name),
                     hjust = "outward",
                     vjust = "outward") +
    theme_graph(background = "white")  + 
  scale_x_reverse()
# weights <- NULL
# weights <- edge_weights_n(graph_tri)
# graph_tri_s <- igraph::simplify(graph_tri)
# colours <- colour_tri_path(graph_tri_s, df$c1)


# mtx <- graph_tri$mtx
# p1 <- paste(mtx[,1], mtx[,2]) 
# p2 <- paste(mtx[,2], mtx[,3])
# idx <- ifelse(duplicated(p1) | duplicated(p2), TRUE, FALSE)
# mtx[!idx,]
# mtx[idx,]
# mtx
# el_p <- paste(el[,1], el[,2], sep = "-")
# el_p[-duplicated(el_p)]
#
# colours <- vector(length = ecount(graph_tri))
# idx <- E(graph_tri)[(.inc("B") & .inc("1")) | (.inc("1") & .inc("Z")) ]
# colours[idx] <- "yes"
# 
# 
# E(graph_tri)[.from("A")]
# E(graph_tri)[.inc("A")]
# 
# 
# edge_column <- df$c1
# el <- get.edgelist(graph_tri_s)
# c1 <- unique(edge_column)
# colours <- vector(length = ecount(graph_tri_s))
# elp <- paste(el[, 1], el[, 2])
# for (i in c1) {
#   for (j in seq(nrow(el)/2)) {
#     if (str_detect(elp[j], i)) { 
#       colours[j] <- i
#       colours[j + nrow(el)/2] <- i
#     }
#   }
# }
# colours
# 
# colours <- colour_tri_path(graph_tri, df$c1)
# igraph::simplify(graph_tri) %>%
## mtx <- graph_tri$mtx
# p1 <- duplicated(paste(mtx[,1], mtx[,2]))
# p2 <- duplicated(paste(mtx[,2], mtx[,3]))
# dup <- ifelse(duplicated(p1) | duplicated(p2), TRUE, FALSE)

# important code

# colours <- vector(length = ecount(graph_tri))
# for(i in seq(nrow(mtx))){
#   colours[igraph::E(graph_tri)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]] <- mtx[i,1]   
# }

# select <- unique(mtx[,1]) %>%
#   paste(collapse = "|")
# if(sum(dup != 0)){
#   for(i in seq(length(E(graph_tri)[dup]))){
#     if(str_detect(graph_tri$el[dup,][i,1], select) == TRUE){
#      colours[E(graph_tri)[dup][i]] <- graph_tri$el[dup,][i,1]
#     } else{
#      colours[E(graph_tri)[dup][i]] <- paste(graph_tri$el[dup,][i,1], graph_tri$el[dup,][i,2])
#     }
#   }
# }
# if(sum(dup != 0)){
#   vector1<- get_dup_edges(graph_tri$el, graph_tri$mtx, dup)
#   vector2 <- colours
#   matches <- tibble(Full = vector1) %>%
#     mutate(Short = str_extract(Full, "\\s(.*)")) %>%
#     mutate(Short = str_trim(Short, side = "left"))
#   for(i in seq(nrow(matches))){
#     idx <- which(vector2 == matches$Short[i])
#     vector2[idx] <- matches$Full[i]
#   }
#   colours <- vector2 
# }


# mtx <- graph_tri$mtx
# dup <- graph_tri$dup
# el <- graph_tri$el
#  colours <- vector(length = ecount(graph_tri))
# for(i in seq(nrow(mtx))){
#    colours[igraph::E(graph_tri)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]] <- mtx[i,1]
#  }
#  select <- unique(mtx[,1]) %>%
#    paste(collapse = "|")
#  if(sum(dup != 0)){
#    for(i in seq(length(E(graph_tri)[dup]))){
#      if(str_detect(graph_tri$el[dup,][i,1], select) == TRUE){
#       colours[E(graph_tri)[dup][i]] <- graph_tri$el[dup,][i,1]
#      } else{
#       colours[E(graph_tri)[dup][i]] <- paste(graph_tri$el[dup,][i,1], graph_tri$el[dup,][i,2])
#      }
#    }
#  }
#  if(sum(dup != 0)){
#    vector1<- get_dup_edges(graph_tri$el, graph_tri$mtx, dup)
#    vector2 <- colours
#    matches <- tibble(Full = vector1) %>%
#      mutate(Short = str_extract(Full, "\\s(.*)")) %>%
#      mutate(Short = str_trim(Short, side = "left"))
#    for(i in seq(nrow(matches))){
#      idx <- which(vector2 == matches$Short[i])
#      vector2[idx] <- matches$Full[i]
#    }
#    colours <- vector2
#  }
# colours