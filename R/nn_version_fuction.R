GetKEdgesForVertex <- function(graph, vertex, k, decreasing=TRUE) {
  edges <- igraph::incident(graph, vertex)
  sort.edges <- edges[order(edges$weight, decreasing = decreasing)]
  k.edges <- sort.edges[1:min(k, length(sort.edges))]
  
  return(k.edges)
}

GetAllWithoutKEdgesForVertex <- function(graph, vertex, k, decreasing=TRUE) {
  edges <- igraph::incident(graph, vertex)
  sort.edges <- edges[order(edges$weight, decreasing = decreasing)]
  k.edges <- sort.edges[1:max(0, min(length(sort.edges) - k, length(sort.edges)))]
  
  return(k.edges)
}

RestoreMnnGraph <- function(graph, m) {
  print("--- Get edges for remove...")
  lists.of.edges <- pblapply(igraph::V(graph), GetAllWithoutKEdgesForVertex, graph = graph, k = m,
                           decreasing = FALSE)
  bad.edges <- unique(Reduce(union, lists.of.edges))
  print("--- Remove edges...")
  corrected.graph <- igraph::delete.edges(graph, igraph::E(graph)[bad.edges])
  
  return(corrected.graph)
}

RestoreKnnGraph <- function(graph, k) {
  print("--- Get edges for saving...")
  lists.of.edges <- pblapply(igraph::V(graph), GetKEdgesForVertex, graph = graph, k = k,
                           decreasing = TRUE)
  good.edges <- unique(Reduce(union, lists.of.edges))
  print("--- Remove edges...")
  corrected.graph <- igraph::subgraph.edges(graph, igraph::E(graph)[good.edges])
  
  return(corrected.graph)
}

#' @export
UpdateNNGraph <- function(graph, p2.objects, clusters, m,
                          graph.type='knn', embeding.type=NULL) {
  corrected.graph <- graph
  print("Union graphs...")
  for (i in 1:length(levels(clusters))) {
    corrected.graph <- igraph::union(corrected.graph, p2.objects[[i]]$graphs$PCA)
    
    edges.graph <- igraph::E(corrected.graph)
    new.weight <- ifelse(is.na(edges.graph$weight_2), edges.graph$weight_1, edges.graph$weight_2)
    igraph::E(corrected.graph)$weight <- new.weight
  }
  if (graph.type == 'knn') {
    print("Restore kNN graph...")
    corrected.graph <- RestoreKnnGraph(corrected.graph, k)
    
    } ifelse(graph.type == 'mnn') {
    print("Restore mNN graph...")
    corrected.graph <- RestoreMnnGraph(corrected.graph, m)
  }
  
  
  return(corrected.graph)
}
