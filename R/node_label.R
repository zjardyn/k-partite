#' Offset node labels 
#'
#' @param graph_bi A bipartite graph object. 
#' @param n The number of characters to offset by.
#' @param offset The character(s) to use as an offset. Use newline for vertical graphs. 
#' @export
offset_node_label <- function(graph_bi, n = 2, offset = " "){
  nodes <- igraph::V(graph_bi)$name
  el <- igraph::get.edgelist(graph_bi)
  lhs <- el[,1] %>%
    as.vector() %>%
    unique() %>%
    paste(collapse = "|")
  rhs <- el[,2] %>%
    as.vector() %>%
    unique() %>%
    paste(collapse = "|")
tibble::tibble(nodes) %>%
  dplyr::mutate(formatted = dplyr::case_when(
    stringr::str_detect(nodes, lhs) ~ paste(nodes, strrep(offset, n)),
    stringr::str_detect(nodes, rhs) ~ paste(strrep(offset, n), nodes),
    TRUE ~ nodes)) %>%
  dplyr::pull(formatted)
}

insert_character <- function(input_string, interval, character) {
  # Split the input string into individual characters
  characters <- strsplit(input_string, "")[[1]]
  # Calculate the number of insertions needed
  num_insertions <- ceiling(length(characters) / interval) - 1
  # Insert the character at the specified interval
  for (i in 1:num_insertions) {
    index <- interval * i + (i - 1)  # Calculate the insertion index
    characters <- c(characters[1:index], character, characters[(index + 1):length(characters)])
  }
  # Combine the characters back into a string
  output_string <- paste(characters, collapse = "")
  return(output_string)
}
#' Insert a newline character into node label after n characters, repeated over string.
#' 
#' @param graph_bi A bipartite graph object.
#' @param n How many lines until a split happens.
#' @export
split_node_label_newline_n <- function(graph_bi, n = 15){
 nodes <- igraph::V(graph_bi)$name
 sapply(nodes, function(x) {ifelse(nchar(x)<=n, x, insert_character(x, interval = n, character = "\n"))}) %>% as.vector()

}
#' Offset label that has already been split with newlines 
#' @param graph_bi 
#' 
#' 
#' 
#' 
#' 
offset_node_label_newline <- function(graph_bi, n = 2, offset = " "){
  nodes <- igraph::V(graph_bi)$name
  el <- igraph::get.edgelist(graph_bi)
  lhs <- el[,1] %>%
    as.vector() %>%
    unique() %>%
    paste(collapse = "|")
  rhs <- el[,2] %>%
    as.vector() %>%
    unique() %>%
    paste(collapse = "|")
tibble::tibble(nodes) %>%
  dplyr::mutate(formatted = dplyr::case_when(
    stringr::str_detect(nodes, lhs) ~ paste(nodes, strrep(offset, n)),
    stringr::str_detect(nodes, rhs) ~ paste(strrep(offset, n), nodes),
    TRUE ~ nodes)) %>%
  dplyr::mutate(formatted= dplyr::case_when(
    stringr::str_detect(nodes, lhs) ~ stringr::str_replace(formatted, "\n", paste("\n", strrep(offset, n))),
    stringr::str_detect(nodes, rhs) ~ stringr::str_replace(formatted, "\n", paste(strrep(offset, n), "\n")),
    TRUE ~ nodes)) %>%
  dplyr::pull(formatted)
}
