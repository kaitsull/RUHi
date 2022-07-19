#' Cluster an mFISH object
#'
#' @author Kaitlin Sullivan
#'
#' @param mFISH An mFISH object
#' @param k Number of clusters
#' @param dmetric Metric for creating distance matrix (see `dist()`)
#' @param p Power of the Minkowski distance (see `dist()`)
#'
#' @return A clustered mFISH object.
#' @export
#'
ruCluster <- function(mFISH, k, dmetric = "euclidean", p = 2){
  #save pca
  pca <- m@attributes$pca
  #run clustering
  npc <- m@attributes$npc
  hc <- hclust(d = dist(pca[,1:npc], method=dmetric, p=p), method = 'ward.D2')
  #cut tree
  clus <- cutree(hc, k=k)
  filt <- data.frame(cluster = clus, id = mFISH@filteredData$id)

  #update object
  #save the id-cluster combo
  others <-  dplyr::filter(mFISH@rawData, !id %in% filt$id)
  others <- dplyr::mutate(others, cluster=paste(m@attributes$filter.by, "neg", sep="_"))
  others <- dplyr::select(others, c(id, cluster))

  #order by id
  clustered <- rbind(filt, others)
  clustered <- dplyr::arrange(clustered, id)


  #add to metadata
  mFISH@metaData <- dplyr::mutate(mFISH@metaData, cluster = clustered$cluster)
  #add to attributes
  mFISH@attributes$hclust_k <- k
  mFISH@attributes$hclust_metric <- dmetric
  mFISH@attributes$hclust_p <- p

  #return object
  mFISH
}
