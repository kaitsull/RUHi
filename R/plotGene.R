#' Plot cluster identity (written by Mark Cembrowski)
#'
#' @param theGene *character* gene to plot.
#' @param xy *character* if =='umap', plot via UMAP dim reduction / if =='tsne', plot via TSNE dim reduction / if =='geog' (or anything else) plot via spatial (geographical) coords
#' @param doNorm *logical* Plot normalized value?
#' @param lo *numeric* If >0, sets low value of plotted range.
#' @param hi *numeric* If >0, sets high value of plotted range.
#' @return ggplot of clustered data
#'
#' @import ggplot2
#'
#' @export
plotGene <- function(theGene,xy='geog',doNorm=F,lo=-1,hi=-1){
  # select data to plot
  if(doNorm){
    pData <- data.frame(toPlot=nData[,theGene])
  }else{
    pData <- data.frame(toPlot=rData[,theGene])
  }
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
  # adjust axis range, if selected
  if(lo>0){
    pData$toPlot[pData$toPlot<lo] <- lo
  }
  if(hi>0){
    pData$toPlot[pData$toPlot>hi] <- hi
  }
  # plot
  gg <- ggplot(pData,aes(x=xAxis,y=yAxis,colour=toPlot)) + geom_point()
  gg <- gg + theme_bw()
  print(gg)
}
