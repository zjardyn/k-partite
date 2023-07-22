colour_tri_path <- function(graph_tri, edge_column){
  el <- get.edgelist(graph_tri)
  c1 <- unique(edge_column)
  colours <- vector(length = ecount(graph_tri))
  elp <- paste(el[, 1], el[, 2])
  for (i in c1) {
    for (j in seq(nrow(el)/2)) {
      if (str_detect(elp[j], i)) {
        colours[j] <- i
        colours[j + nrow(el)/2] <- i
      }
    }
  }
  colours
}

colour_tri_path2 <- function(graph_tri, el, mtx, dup){
  mtx <- graph_tri$mtx
  dup <- graph_tri$dup
  el <- graph_tri$el
  colours <- vector(length = ecount(graph_tri))
  for(i in seq(nrow(mtx))){
     colours[igraph::E(graph_tri)[(.inc(mtx[i,1]) & .inc(mtx[i,2])) | (.inc(mtx[i,2]) & .inc(mtx[i,3]))]] <- mtx[i,1]
   }
   select <- unique(mtx[,1]) %>%
     paste(collapse = "|")
   if(sum(dup != 0)){
     for(i in seq(length(E(graph_tri)[dup]))){
       if(str_detect(graph_tri$el[dup,][i,1], select) == TRUE){
        colours[E(graph_tri)[dup][i]] <- graph_tri$el[dup,][i,1]
       } else{
        colours[E(graph_tri)[dup][i]] <- paste(graph_tri$el[dup,][i,1], graph_tri$el[dup,][i,2])
       }
     }
   }
   if(sum(dup != 0)){
     vector1<- get_dup_edges(graph_tri$el, graph_tri$mtx, dup)
     vector2 <- colours
     matches <- tibble(Full = vector1) %>%
       mutate(Short = str_extract(Full, "\\s(.*)")) %>%
       mutate(Short = str_trim(Short, side = "left"))
     for(i in seq(nrow(matches))){
       idx <- which(vector2 == matches$Short[i])
       vector2[idx] <- matches$Full[i]
     }
     colours <- vector2
   }
  colours
}


# colours <- vector(length = ecount(graph_bi)) # FALSE element vector length of edges
# ## Method 1.
# # i iterates over the number of edges in graph_bi
# for (i in seq(ecount(graph_bi))){
#   # included edges with respect to their index in
#   idx <- igraph::E(graph_bi)[.inc(el_bi[i,1]) & .inc(el_bi[i,2])]
#   for (j in seq(colours[idx])){
#     if (colours[idx][j] != FALSE){
#       colours[idx][j] <- paste(colours[idx][j], column[i])
#     } else {
#       colours[idx][j] <- column[i]
#     }
#   }
# }
# colours_u <- sapply(strsplit(colours, " "), trimws) %>%
#   sapply(unique) %>%
#   sapply(paste, collapse = " ")
# el_bi_p <- paste(el_bi[,1], el_bi[,2]) # edge
# el_bi_s <- get.edgelist(igraph::simplify(graph_bi))
# el_bi_s_p <- paste(el_bi_s[,1], el_bi_s[,2])
# idx_s <- match(el_bi_s_p, el_bi_p)
# colours <- colours_u[idx_s]
