#' Plot violin of gene expression (written by Mark Cembrowski)
#'
#' @param theGene *character* Gene to plot
#' @param doNorm *logical* Plot normalized value?
#' @param alpha *numeric* ransparency of individual cell data points. If <=0, turns individual points off. If =1, points full opaque.
#' @return ggplot of clustered data
#'
#' @import ggplot2
#'
#' @export
plotViolin <- function(theGene,doNorm=F,alpha=0.2){
  # select data to plot
  if(doNorm){
    pData <- data.frame(toPlot=nData[,theGene])
  }else{
    pData <- data.frame(toPlot=rData[,theGene])
  }
  pData$clust <- mData$clust

  # plot
  gg <- ggplot(pData,aes(x=clust,y=toPlot,fill=clust)) + geom_violin(scale = "width")
  if(alpha>0){gg <- gg + geom_jitter(alpha=alpha)}
  gg <- gg + theme_bw()
  print(gg)
}
