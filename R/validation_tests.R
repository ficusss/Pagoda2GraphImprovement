
CountEdgeChangesForVertex <- function(g1, g2, vertex) {
  c1 <- sum(g1[vertex] > 0)
  c2 <- sum(g2[vertex] > 0)
  
  return(c(c1, c2))
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

CountEdgesVertexCluster <- function(g, vertex, clusters.name) {
  all_edges <- igraph::incident(g, vertex)
  
}

