#' @export
PlotPagoda <- function(data, embedding.name, cluster.name=NULL, size=0.3, alpha=0.5) {
  embeddings <- data$embeddings$PCA[[embedding.name]]
  clusters <- data$clusters$PCA[[cluster.name]]
  
  ggplot2::ggplot() + 
    ggplot2::geom_point(ggplot2::aes(x = embeddings[,1], y = embeddings[,2], color=clusters),
                        size=size,alpha=alpha)
}

#' @description Similar to basicP2proc, but with more parameters
#' @export
GetPagoda <- function (cm, n.cores = 4, clustering.type = "infomap", embeding.type = "tSNE", 
                       tsne.iter.num = 1000, verbose = TRUE, n.pcs = 100, distance = "cosine", 
                       trim = 5, n.odgenes = 1000,  ...) 
{
  r <- pagoda2::Pagoda2$new(cm, modelType = "plain", trim = trim, 
                            n.cores = n.cores, verbose = verbose, ...)
  r$adjustVariance(plot = F, do.par = F, gam.k = 10, verbose = verbose)
  r$calculatePcaReduction(nPcs = n.pcs, n.odgenes = n.odgenes, maxit = 1000)
  
  r$makeKnnGraph(k = 30, type = "PCA", center = T, distance = "cosine", 
                 weight.type = "none", verbose = verbose)
  if (clustering.type == "infomap") {
    r$getKnnClusters(method = igraph::infomap.community, type = "PCA", name = "infomap")
  } else if (clustering.type == "multilevel") {
    r$getKnnClusters(method = igraph::multilevel.community, type = "PCA", name = "multilevel")
  } else stop("Unknown clustering  type")
  
  if ("largeVis" %in% embeding.type) {
    r$getEmbedding(type = "PCA", embeddingType = "largeVis")
  }
  
  if ("tSNE" %in% embeding.type) {
    r$getEmbedding(type = "PCA", perplexity = 30, embeddingType = "tSNE", 
                   max_iter = tsne.iter.num)
  }
  
  return(r)
}

#' @export
ClusterNeighborhood <- function(index, clusters, p2, order=1, mindist=0) {
  tmp_list <- list()
  # выбираем кластер
  cluster <- names(clusters[clusters==index])

  # составляем список из всех соседей для каждой вершины кластера
  tmp_list <- unlist(igraph::neighborhood(p2$graphs$PCA, nodes = cluster, order=order, mindist=mindist))

  # удаляем повторяющиеся вершины
  tmp_list <- names(tmp_list[!duplicated(tmp_list)])
  
  list(cluster=cluster, expand_cluster=tmp_list, add_vertex=tmp_list[!(tmp_list %in% cluster)])
}

#' @export
GetDegreeConnectivity <- function(graph, vertex.name) {
  result <- list()
  
  for (v in vertex.name$add_vertex) {
    tmp <- names(graph[[v]][[1]]) %in% vertex.name$cluster
    names.cluster.vertex <- names(graph[[v]][[1]])[tmp]
    
    tmp <- names(graph[[v]][[1]]) %in% vertex.name$add_vertex
    names.add.vertex <- names(graph[[v]][[1]])[tmp]
    
    tmp <- !(names(graph[[v]][[1]]) %in% vertex.name$expand_cluster)
    names.other.vertex <- names(graph[[v]][[1]])[tmp]
    res <- NULL
    res[[v]] <- list(
      names.cluster.vertex=names.cluster.vertex,
      names.add.vertex=names.add.vertex,
      names.other.vertex=names.other.vertex)
    result <- c(result, res)
  }
  
  return(result)
}

#' @description Remove unwanted connections from the graph
#' @export
RemoveConnections <- function(graph, clusters.name, vertex.connectivity,
                              update.info=FALSE, embeding.type=NULL) {
  assertthat::assert_that(length(vertex.connectivity) == length(clusters.name),
                          msg = "That data must have the same length")
  
  matrix.graph <- igraph::as_adj(graph)
  for (cluster.index in length(vertex.connectivity)) {
    
    cluster <- clusters.name[[cluster.index]][[1]]
    add <- clusters.name[[cluster.index]][[3]]
    tmp <- !(matrix.graph@Dimnames[[1]] %in% clusters.name[[cluster.index]][[2]])
    other <- matrix.graph@Dimnames[[1]][tmp]
    groups.name <- list(cluster, add, other)

    for (index.group in length(groups.name)) {
      # рассматриваем каждую вершину из окрестности кластера
      for (curr.row.name in names(vertex.connectivity[[cluster.index]])) {
        # вершина с именами ее окрестности из гразны групп
        curr.vertex <- vertex.connectivity[[cluster.index]][[curr.row.name]]
        # количество соседей которое необходимо оставить для текущей верершины
        # из определенной группы (кластера/окрестности/остального)
        count.connect.vertex <- length(curr.vertex[[index.group]])
        # вершины кластера/окрестности/остальные в графе
        groups.vertex <- matrix.graph[curr.row.name, groups.name[[index.group]]]
        # имена всех вершин с которыми иеются связи у текущей
        all.connection <- names(groups.vertex[groups.vertex > 0])
        
        assertthat::assert_that(length(all.connection) >= count.connect.vertex,
                                msg = "Error - Deleted vertex!")
        
        # логическая индексация вершин которые нужно оставить или удалить
        logical.index <- sample(c(logical(count.connect.vertex), !logical(length(all.connection) - count.connect.vertex)))
        # вектор вершин котрые подлежат удалению
        deffect.conection <- all.connection[logical.index]
        # лишние связи убираем (зануляем)
        for (curr.col.name in deffect.conection) {
          matrix.graph[curr.row.name, curr.col.name] <- 0
          matrix.graph[curr.col.name, curr.row.name] <- 0
        }
      }
    }
  }

  return(GetPagoda(matrix.graph, embeding.type = embeding.type))
}

#' @export
UpdateGraph <- function(clusters, p2.objects, graph, clusters.name, vertex.connectivity,
                        embeding.type=NULL, deleted=FALSE) {
  corrected.graph <- graph
  for (i in 1:length(levels(clusters))) {
    if (deleted == TRUE) {
      corrected.graph <- igraph::delete_vertices(corrected.graph, clusters.name[[i]]$cluster)
    }

    corrected.graph <- igraph::union(corrected.graph, p2.objects[[i]]$graphs$PCA)
  }
  
  return(RemoveConnections(corrected.graph, clusters.name, vertex.connectivity, deleted, embeding.type))
}

#' @export
EmbedGraph <- function(graph, M=1, gamma=1, alpha=0.1, sgd_batches=1e8,
                       seed=1, verbose=TRUE, n.cores=1) {
  wij <- igraph::as_adj(graph, attr='weight');
  coords <- largeVis::projectKNNs(wij = wij, dim=2, verbose = verbose,
                                  sgd_batches = sgd_batches,gamma=gamma, M=M,
                                  seed=seed, alpha=alpha, rho=1, threads=n.cores)
  colnames(coords) <- igraph::V(graph)$name
  return(t(coords))
}

#' @export
GetBaseClusters <- function(base.graph, new.graph, cluster.type) {
  l1 <- base.graph$clusters$PCA[[1]]
  l2 <- new.graph$clusters$PCA[[1]]
  assertthat::are_equal(l1, l2)
  
  z <- base.graph$clusters$PCA[[cluster.type]]
  names(z) <- names(base.graph$clusters$PCA[[cluster.type]])
  
  base.clusters <- NA
  for (i in 1:length(z)) {
    curr.cell <- names(new.graph$graphs$PCA[[i]])
    base.clusters[i] <- z[curr.cell]
  }
  
  return(as.factor(base.clusters))
}