#' Plot cluster identity (written by Mark Cembrowski)
#'
#' @param xy *character* if =='umap', plot via UMAP dim reduction / if =='tsne', plot via TSNE dim reduction / if =='geog' (or anything else) plot via spatial (geographical) coords
#' @return ggplot of clustered data
#'
#' @import ggplot2
#'
#' @export
plotCluster <- function(xy='geog'){
  # select cluster ID
  pData <- data.frame(clust=mData$clust)
  # select XY coords
  if(xy=='umap'){
    pData$xAxis <- mData$umap1
    pData$yAxis <- mData$umap2
  }else if(xy=='tsne'){
    pData$xAxis <- mData$tsne1
    pData$yAxis <- mData$tsne2
  }else{
    pData$xAxis <- mData$X
    pData$yAxis <- mData$Y
  }
  # plot
  gg <- ggplot(pData,aes(x=xAxis,y=yAxis,colour=clust)) + geom_point()
  gg <- gg + theme_bw()
  print(gg)
}
