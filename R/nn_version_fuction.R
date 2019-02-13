GetKEdgesForVertex <- function(graph, vertex, k, clusters.name, decreasing=TRUE, mode=F) {
  edges <- igraph::incident(graph, vertex)
  sort.edges <- edges[order(edges$weight, decreasing = decreasing)]
  count.edges <- min(k, length(sort.edges))
  
  if (mode){
    count.edges.cluster <- round(count.edges * 0.75)
    count.edges.other <- count.edges - count.edges.cluster
    
    cluster.index <- FindIndexClusterForVertex(igraph::V(graph)[vertex], clusters.name)
    from.cluster <- igraph::ends(graph, sort.edges)[,2] %in% clusters.name[[cluster.index]]$cluster
    
    edges.cluster <- sort.edges[from.cluster]
    edges.other <- sort.edges[!from.cluster]
    
    #print('------------')
    #print(cat('count.edges = ', count.edges))
    #print(cat('count.edges.cluster = ', count.edges.cluster))
    #print(cat('count.edges.other = ', count.edges.other))
    #print(cat('length(sort.edges) = ', length(sort.edges)))
    #print(cat('length(edges.cluster) = ', length(edges.cluster)))
    #print(cat('length(edges.other) = ', length(edges.other)))
    
    if (length(edges.other) <= count.edges.other || !count.edges.other) {
      k.edges <- sort.edges[1:count.edges]
      #print("q1")
    } else {
      res.edges.cluster <- edges.cluster[1:count.edges.cluster]
      res.edges.other <- edges.cluster[1:count.edges.cluster]
      k.edges <- igraph::union(res.edges.cluster, res.edges.other)
      #print("q2")
    }
    #print('------------')
  } else {
    k.edges <- sort.edges[1:count.edges]
  }
  return(k.edges)
}

GetAllWithoutKEdgesForVertex <- function(graph, vertex, k, decreasing=TRUE) {
  edges <- igraph::incident(graph, vertex)
  sort.edges <- edges[order(edges$weight, decreasing = decreasing)]
  k.edges <- sort.edges[1:max(0, min(length(sort.edges) - k, length(sort.edges)))]
  
  return(k.edges)
}

RestoreMnnGraph <- function(graph, m, n.cores) {
  print("--- Get edges for remove...")
  lists.of.edges <- pbapply::pblapply(igraph::V(graph), GetAllWithoutKEdgesForVertex, graph = graph, 
                                      k = m, decreasing = FALSE, cl = n.cores)
  print("--- Get unique...")
  bad.edges <- unique(Reduce(union, lists.of.edges))
  print("--- Remove edges...")
  corrected.graph <- igraph::delete.edges(graph, igraph::E(graph)[bad.edges])
  
  return(corrected.graph)
}

RestoreKnnGraph <- function(graph, k, clusters.name, n.cores) {
  print("--- Get edges for saving...")
  lists.of.edges <- pbapply::pblapply(igraph::V(graph), GetKEdgesForVertex, graph = graph, 
                                      k = k, clusters.name = clusters.name, decreasing = TRUE,
                                      cl = n.cores)
  print("--- Get unique...")
  suppressWarnings(
    good.edges <- Reduce(igraph::union, lists.of.edges)
  )
  print("--- Remove edges...")
  corrected.graph <- igraph::subgraph.edges(graph, igraph::E(graph)[good.edges])
  
  return(corrected.graph)
}

#' @export
UpdateNNGraph <- function(graph, p2.objects, clusters, clusters.name, k,
                          graph.type='knn', n.cores=2, embeding.type=NULL) {
  corrected.graph <- graph
  print("Union graphs...")
  for (i in 1:length(levels(clusters))) {
    corrected.graph <- igraph::union(corrected.graph, p2.objects[[i]]$graphs$PCA)
    
    edges.graph <- igraph::E(corrected.graph)
    
    #new.weight <- ifelse(is.na(edges.graph$weight_2), edges.graph$weight_1, edges.graph$weight_2)
    
    #new.weight <- pmax(edges.graph$weight_1, edges.graph$weight_2, na.rm=T)
    
    w1 <- replace(edges.graph$weight_1, is.na(edges.graph$weight_1), 0)
    w2 <- replace(edges.graph$weight_2, is.na(edges.graph$weight_2), 0)
    new.weight <- w1 + w2
    #new.weight <- scales::rescale(new.weight, to=c(0.001, 0.99))
    
    igraph::E(corrected.graph)$weight <- new.weight
  }
  new.weight <- igraph::E(corrected.graph)$weight
  new.weight <- scales::rescale(new.weight, to=c(0.001, 0.99))
  igraph::E(corrected.graph)$weight <- new.weight
  
  if (graph.type == 'knn') {
    print("Restore kNN graph...")
    corrected.graph <- RestoreKnnGraph(corrected.graph, k, clusters.name, n.cores = n.cores)
    } else if(graph.type == 'mnn') {
    print("Restore mNN graph...")
    corrected.graph <- RestoreMnnGraph(corrected.graph, k, n.cores = n.cores)
  }
  
  
  return(corrected.graph)
}

#' @export
KnnGraphToAdjList <- function(graph) {
  edge.list.fact <- igraph::as_edgelist(graph) %>% conos::as_factor()
  edge.list <- matrix(edge.list.fact$values, ncol = 2)
  n.nodes <- length(igraph::V(graph))
  
  adj.list <- mapply(c, conos:::splitVectorByNodes(edge.list[, 1], edge.list[, 2], n.nodes), 
                     conos:::splitVectorByNodes(edge.list[, 2], edge.list[, 1], n.nodes)) %>% 
    lapply(unlist) %>% lapply(`-`, 1)
  
  dists <- mapply(c, conos:::splitVectorByNodes(igraph::E(graph)$weight, edge.list[, 2], n.nodes), 
                  conos:::splitVectorByNodes(igraph::E(graph)$weight, edge.list[, 1], n.nodes)) %>% 
    lapply(unlist) %>% lapply(function(x) 1 - x) %>% setNames(edge.list.fact$levels)
  
  edge.orders <- lapply(dists, order)
  dists <- lapply(dists, sort)
  adj.list <- mapply(function(e, o) e[o], adj.list, edge.orders) %>% 
    setNames(edge.list.fact$levels)
  
  return(list(idx=adj.list, dist=dists))
}