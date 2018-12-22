
CountEdgeChangesForVertex <- function(g1, g2, vertex) {
  c1 <- sum(g1[vertex] > 0)
  c2 <- sum(g2[vertex] > 0)
  
  return(list(vertex, c1, c2))
}

#' @export
CountEdgeChanges <- function(g1, g2) {
  vertices <- igraph::V(g1)
  assertthat::are_equal(vertices, igraph::V(g2))
  
  return(pbapply::pblapply(names(vertices), CountEdgeChangesForVertex, g1 = g1, g2 = g2))
}

#' @export
GetModularityInfo <- function(g, clustering.type = "infomap") {
  r <- NULL
  if ("infomap" %in% clustering.type) {
    wtc <- igraph::infomap.community(g)
    r$infomap <- igraph::modularity(g, igraph::membership(wtc),
                                    weights = igraph::E(g)$weight)
  }
  if ("multilevel" %in% clustering.type) {
    wtc <- igraph::multilevel.community(g)
    r$multilevel <- igraph::modularity(g, igraph::membership(wtc),
                                    weights = igraph::E(g)$weight)
  }
  
  return(r)
}

#' @export
GetModularityForGraphs <- function(graph.list, clustering.type = "infomap") {
  # r <- NULL
  # for(i in length(graph.list)) {
  #   r$paste0("g", i) <- GetModularityInfo(graph.list[[i]], clustering.type)
  # }
  
  r <- pblapply(graph.list, GetModularityInfo, clustering.type = clustering.type)
  
  return(r)
}

#' @export
EdgeСhangesForGraphs <- function(g1, g2) {
  incident_vertex <- igraph::ends(g1, igraph::E(g1))
  edges1 <- paste(incident_vertex[, 1], incident_vertex[, 2])
  incident_vertex <- igraph::ends(g2, igraph::E(g2))
  edges2 <- paste(incident_vertex[, 1], incident_vertex[, 2])
  
  count.intersect <- length(intersect(edges1, edges2))
  count.union <- length(union(edges1, edges2))
  
  return(count.intersect / count.union)
}

#' @export
EdgeСhangesForCluster <- function(g1, g2, cluster.names) {
  edges <- igraph::incident(g1, cluster.names)
  incident_vertex <- igraph::ends(g1, edges)
  edges1 <- paste(incident_vertex[, 1], incident_vertex[, 2])
  edges <- igraph::incident(g2, cluster.names)
  incident_vertex <- igraph::ends(g2, edges)
  edges2 <- paste(incident_vertex[, 1], incident_vertex[, 2])
  
  count.intersect <- length(intersect(edges1, edges2))
  count.union <- length(union(edges1, edges2))
  
  return(count.intersect / count.union)
}

#' @export
SummaryEdgeChanges <- function(g1, g2, clusters.name) {
  r <- NULL
  r$all.graph <- EdgeСhangesForGraphs(g1, g2)
  
  for (i in 1:length(clusters.name)) {
    r[[paste0("cluster", i)]] <- EdgeСhangesForCluster(g1, g2, clusters.name[[i]]$cluster)
  }
  
  return(r)
}

CountEdgesVertexCluster <- function(g, vertex, cluster.names) {
  edges <- igraph::incident(g, vertex)
  incident.vertex <- igraph::ends(g, edges)[, 2]
  
  return( sum(incident.vertex %in% cluster.names) )
}
