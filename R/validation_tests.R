
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