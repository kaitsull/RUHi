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
ruCluster <- function(mFISH, k, npc = 8, dmetric = "euclidean", p = 2){
  #save pca
  pca <- mFISH@attributes$pca
  #run clustering
  hc <- hclust(d = dist(pca[,1:npc], method=dmetric, p=p), method = 'ward.D2')
  #cut tree
  clus <- cutree(hc, k=k)

  #update object
  #save the id-cluster combo
  fildat <- dplyr::mutate(mFISH@filteredData, cluster = as.factor(clus))
  met <- dplyr::mutate(mFISH@metaData, ifelse(fil==T,
                                       fildat$cluster,
                                       paste(mFISH@attributes$filter.by, "neg", sep = "_")))
  #add to metadata
  mFISH@metaData <- met
  #add to attributes
  mFISH@attributes$hclust_k <- k
  mFISH@attributes$hclust_metric <- dmetric
  mFISH@attributes$hclust_p <- p

  #return object
  mFISH
}
