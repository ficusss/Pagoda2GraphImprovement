#' @export
GetKEdgesForVertex <- function(graph, vertex, k, edge.graph, decreasing=TRUE) {
  edges <- igraph::incident(pagoda_data$graphs$PCA, vertex)
  print(edges)
  print(edges$weight)
  edges <- edge.graph[igraph::incident(graph, vertex)]
  print(edges)
  print(edges$weight)
  edges <- edge.graph[edges]
  print(edges)
  print(edges$weight)
  sort.edges <- edges[order(edges$weight, decreasing = decreasing)]
  
  len <- length(sort.edges)
  if (decreasing)
    k <- min(k, len)
  else
    k <- max(0, min(len - k, len))
  
  return(sort.edges[1:k])
}

#' @export
GetEdgesForCluster <- function(graph, vertices, k, edge.graph, decreasing=TRUE) {
  lists.of.edges <- lapply(vertices, GetKEdgesForVertex, graph = graph, k = k,
                           edge.graph = edge.graph, decreasing = decreasing)
  res <- unique(Reduce(union, lists.of.edges))
  
  return(res)
}

#' @export
RestoreKnnGraph <- function(graph, clusters.vertices, k) {
  edge.graph <- igraph::E(graph)
  bad.edges <- GetEdgesForCluster(graph, clusters.vertices, k, edge.graph, decreasing=FALSE)
  corrected.graph <- igraph::delete.edges(graph, edge.graph[bad.edges])
  
  return(corrected.graph)
}

#' @export
UpdateKnnGraph <- function(graph, p2.objects, clusters, clusters.name, embeding.type=NULL) {
  corrected.graph <- graph
  for (i in 1:length(levels(clusters))) {
    corrected.graph <- igraph::union(corrected.graph, p2.objects[[i]]$graphs$PCA)
    vertices.graph <- igraph::V(corrected.graph)
    vertices.cluster <- vertices.graph[clusters.name[[i]]$expand_cluster]
    corrected.graph <- RestoreKnnGraph(corrected.graph, vertices.cluster, 20)
  }
  
  return(corrected.graph)
}