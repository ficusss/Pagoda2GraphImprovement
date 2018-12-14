#' @export
GetKEdgesForVertex <- function(graph, vertex, k, decreasing=TRUE) {
  edges <- igraph::incident(graph, vertex)
  sort.edges <- edges[order(edges$weight, decreasing = decreasing)]
  
  len <- length(sort.edges)
  if (decreasing)
    k <- min(k, len)
  else
    k <- max(0, min(len - k, len))
  
  return(sort.edges[1:k])
}

#' @export
RestoreMnnGraph <- function(graph, m) {
  print("--- Get edges for remove...")
  lists.of.edges <- pblapply(igraph::V(graph), GetKEdgesForVertex, graph = graph, k = m,
                           decreasing = FALSE)
  bad.edges <- unique(Reduce(union, lists.of.edges))
  print("--- Remove edges...")
  corrected.graph <- igraph::delete.edges(graph, igraph::E(graph)[bad.edges])
  
  return(corrected.graph)
}

#' @export
UpdateMnnGraph <- function(graph, p2.objects, clusters, clusters.name, m, embeding.type=NULL) {
  corrected.graph <- graph
  print("Union graphs...")
  for (i in 1:length(levels(clusters))) {
    corrected.graph <- igraph::union(corrected.graph, p2.objects[[i]]$graphs$PCA)
    
    edges.graph <- igraph::E(corrected.graph)
    new.weight <- ifelse(is.na(edges.graph$weight_2), edges.graph$weight_1, edges.graph$weight_2)
    igraph::E(corrected.graph)$weight <- new.weight
  }
  print("Restore mNN graph...")
  corrected.graph <- RestoreMnnGraph(corrected.graph, m)
  
  return(corrected.graph)
}

#' @export
RestoreKnnGraph <- function(graph, k) {
  print("--- Get edges for saving...")
  lists.of.edges <- lapply(igraph::V(graph), GetKEdgesForVertex, graph = graph, k = k,
                           decreasing = TRUE)
  good.edges <- unique(Reduce(union, lists.of.edges))
  print("--- Remove edges...")
  corrected.graph <- igraph::subgraph.edges(graph, igraph::E(graph)[good.edges])
  
  return(corrected.graph)
}

#' @export
UpdateKnnGraph <- function(graph, p2.objects, clusters, clusters.name, k, embeding.type=NULL) {
  corrected.graph <- graph
  print("Union graphs...")
  for (i in 1:length(levels(clusters))) {
    corrected.graph <- igraph::union(corrected.graph, p2.objects[[i]]$graphs$PCA)
    
    edges.graph <- igraph::E(corrected.graph)
    new.weight <- ifelse(is.na(edges.graph$weight_2), edges.graph$weight_1, edges.graph$weight_2)
    igraph::E(corrected.graph)$weight <- new.weight
  }
  print("Restore kNN graph...")
  corrected.graph <- RestoreKnnGraph(corrected.graph, k)
  
  return(corrected.graph)
}