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
GetEdgesForCluster <- function(graph, vertices, k, decreasing=TRUE) {
  lists.of.edges <- lapply(vertices, GetKEdgesForVertex, graph = graph, k = k,
                           decreasing = decreasing)
  res <- unique(Reduce(union, lists.of.edges))
  
  return(res)
}

#' @export
RestoreMnnGraph <- function(graph, vertices, m) {
  print("--- --- Get edges for remove...")
  lists.of.edges <- lapply(vertices, GetKEdgesForVertex, graph = graph, k = m,
                           decreasing = FALSE)
  bad.edges <- unique(Reduce(union, lists.of.edges))
  print("--- --- Remove edges...")
  corrected.graph <- igraph::delete.edges(graph, igraph::E(graph)[bad.edges])
  
  return(corrected.graph)
}

#' @export
UpdateIncorrectMnnGraph <- function(graph, p2.objects, clusters, clusters.name, m, embeding.type=NULL) {
  corrected.graph <- graph
  vertices.graph <- igraph::V(corrected.graph)
  for (i in 1:length(levels(clusters))) {
    print(paste("Processing cluster number -", i))
    print("--- Union graph with clusters graph...")
    corrected.graph <- igraph::union(corrected.graph, p2.objects[[i]]$graphs$PCA)
    
    edges.graph <- igraph::E(corrected.graph)
    new.weight <- ifelse(is.na(edges.graph$weight_2), edges.graph$weight_1, edges.graph$weight_2)
    igraph::E(corrected.graph)$weight <- new.weight
    vertices.cluster <- vertices.graph[clusters.name[[i]]$expand_cluster]
    
    print("--- Restore mNN graph...")
    corrected.graph <- RestoreMnnGraph(corrected.graph, vertices.cluster, m)
  }
  
  return(corrected.graph)
}

RestoreKnnGraph <- function(graph, m) {
  bad.edges <- GetEdgesForCluster(graph, clusters.vertices, m, decreasing=FALSE)
  corrected.graph <- igraph::delete.edges(graph, igraph::E(graph)[bad.edges])
  
  return(corrected.graph)
}

UpdateKnnGraph <- function(graph, p2.objects, clusters, clusters.name, k, embeding.type=NULL) {
  corrected.graph <- graph
  for (i in 1:length(levels(clusters))) {
    corrected.graph <- igraph::union(corrected.graph, p2.objects[[i]]$graphs$PCA)
    edges.graph <- igraph::E(corrected.graph)
    new.weight <- ifelse(is.na(edges.graph$weight_2), edges.graph$weight_1, edges.graph$weight_2)
    igraph::E(corrected.graph)$weight <- new.weight
  }
  corrected.graph <- RestoreKnnGraph(corrected.graph, vertices.cluster, k)
  
  return(corrected.graph)
}