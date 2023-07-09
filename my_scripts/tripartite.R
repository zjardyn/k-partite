library(btvis)
library(igraph)
library(tidyverse)
library(ggraph)

df <- tibble(c1 = c("A", "A", "B", "B", "B", "C", "C", "D"),
             c2 = c(1, 2, 1, 2, 3, 3, 4, 4),
             c3 = c("X","Y", "Z", "Y", "Z", "Z", "X", "Y"))

mx <- df %>%
  select(c1, c2, c3) %>%
  as.matrix()
el <- rbind(mx[,1:2], mx[,2:3])
graph_tri <- graph_from_edgelist(el, directed = FALSE)
layers <- ceiling(match(V(graph_tri)$name, mx)/nrow(mx))
l <- layout_with_sugiyama(graph_tri, layers)

# choose the level of nodes to colour, 1, 2, 3
nodes <- V(graph_tri)$name
length(nodes)

# selection <- mx[mx[,1] == "B",]
mx[,1:2]
mx[,2:3]


el2 <- get.edgelist(graph_tri)
c1 <-unique(df$c1)

colours <- vector(length = ecount(graph_tri))
el3 <- paste(el2[,1], el2[,2])

for(i in c1){
  for(j in seq(nrow(el2)/2)){
    if(str_detect(el3[j], i)){
      colours[j] <- i
      colours[j + nrow(el2)/2] <- i
    }
  }
}
colours

# CHATGPT METHOD 1
# Get the edge list
el2 <- get.edgelist(graph_tri)

# Get the unique node labels
nodes <- unique(unlist(el2))

# Create an empty vector to store edge colors
colours <- vector(length = length(E(graph_tri)))

# Loop through each node and assign colors to corresponding edges
for (i in nodes) {
  node_edges <- which(str_detect(paste(el2[, 1], el2[, 2]), i))
  colours[node_edges] <- i
}

colours

# CHATGPT METHOD 2
# Create a color vector for the edges based on the nodes they connect to
colours <- rep(NA, ecount(graph_tri))

# Loop through each node
for (node in unique(unlist(mx))) {
  # Get the edges connected to the current node
  node_edges <- E(graph_tri)[from(node) | to(node)]

  # Assign a color to the edges connected to the current node
  colours[node_edges] <- node
}

# Reverse Order!
colours <- rep(NA, ecount(graph_tri))
for (node in rev(unique(unlist(mx)))) {
  # Get the edges connected to the current node
  node_edges <- E(graph_tri)[from(node) | to(node)]

  # Assign a color to the edges connected to the current node
  colours[node_edges] <- node
}

# GHAT GPT METHOD 3
# Create a color vector for the edges based on the nodes they connect to
edge_colors <- rep(NA, ecount(graph_tri))

# Calculate edge betweenness centrality
edge_betweenness <- edge_betweenness(graph_tri)

# Identify unique edge labels for overlapping edges
unique_labels <- make.unique(as.character(edge_betweenness), sep = "_")

# Assign the unique labels as colors to the edges
E(graph_tri)$colour <- unique_labels


# CHATGPT METHOD 4
el2 <- get.edgelist(graph_tri)
c1 <- unique(df$c1)

colours <- vector(length = ecount(graph_tri))
el3 <- paste(el2[, 1], el2[, 2])

overlapping <- duplicated(el3) | duplicated(el3, fromLast = TRUE)  # Identify overlapping edges

for (i in c1) {
  for (j in seq(nrow(el2)/2)) {
    if (str_detect(el3[j], i) && !overlapping[j]) {  # Skip overlapping edges
      colours[j] <- i
      colours[j + nrow(el2)/2] <- i
    }
  }
}

overlapping
ol <- unique(el3[overlapping]) %>%
  paste(collapse = "|")
mxp <- paste(mx[,1], mx[,2], mx[,3])
strings <- mxp[str_detect(mxp, ol)]

strsplit(mxp[str_detect(mxp, ol)], " ")

vectors <- data.frame(Vector = c("A 2 Y", "B 2 Y", "B 3 Z", "C 3 Z"))

# Separate the elements, values, and letters using tidyr::separate
vectors <- vectors %>%
  separate(Vector, into = c("Element", "Value", "Letter"), sep = " ")

vectors <- vectors %>%
  unite(New, Value, Letter, sep = "")

vector <- aggregate(data = vectors, Element~., FUN=paste, collapse="")
vector <- paste(vector$Element, vector$New)


graph_tri %>%
  ggraph(layout = l$layout[,2:1]) +
    geom_node_point() +
    geom_edge_link(aes(colour = colours)) +
      geom_node_text(aes(label = name),
                     hjust = "outward",
                     vjust = "outward") +
    theme_graph(background = "white")  +
  scale_x_reverse()


# E(graph_tri)[.from("B")]
# E(graph_tri)[.to("1")]
# E(graph_tri)[.inc("1")]

#
# E(graph_tri)
# colours <- vector(length = ecount(graph_tri))
# colours[1] <- TRUE
