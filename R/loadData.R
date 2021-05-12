#' Load mFISH data (written by Mark Cembrowski)
#'
#' @param anm *character vector.* If length>0, select only the animals present in this vector.
#' @param reg *character vector.* If length>0, select only the regions ant','int','pos' present in this vector.
#' @param excThres *numeric* Slc17a7 cutoff value for removing non-excitatory cells.
#' @param clust *numeric* Number of clusters
#' @param perplexity *numeric* Perplexity of tSNE visualization.
#' @return A normalized dataframe containing the quantified mFISH.
#'
#'
#'

loadData <- function(anm=c(),reg=c(),excThres=1e-5,nClust=4,perplexity=30){
  # load packages
  library(reshape2)
  library(ggplot2)
  library(Rtsne)
  library(umap)
  # load raw data (henceforth, rData)
  rData <- read.table('allCla.txt',sep='\t',header=T)
  # print number of initial cells
  print(paste('total number of initial cells is',nrow(rData)))

  # screen by animal number, if selected
  if(length(anm)>0.1){
    rData <- subset(rData,rData$anm%in%anm)
    print(paste('total number of cells after animal selection is',nrow(rData)))
  }
  # screen by section region, if selected
  if(length(reg)>0.1){
    rData <- subset(rData,rData$reg%in%reg)
    print(paste('total number of cells after region selection is',nrow(rData)))
  }
  # for ease of coding, assign unique ID that can track cells across raw data,
  # metadata, nad processed data
  rUniqueId <- paste(paste(rData$anm,rData$reg,sep='_'),rData$ind,sep='')
  rUniqueId <- paste('anm',rUniqueId,sep='')
  rownames(rData) <- rUniqueId
  # screen by Slc17a7 expression
  if(excThres>0){
    rData <- subset(rData,Slc17a7>excThres)
    print(paste('total number of cells after exc selection is',nrow(rData)))
  }
  # tidy up for visualizations. This just offsets X values so that sections
  # can be viewed side-by-side in same graph
  rData$X[rData$reg=='ant'] <- rData$X[rData$reg=='ant']-2000
  rData$X[rData$reg=='pos'] <- rData$X[rData$reg=='pos']+2000
  # define metadata (mData), raw data (rData), and normalized
  # data (nData)
  mData <- rData[,c(1:5)]
  rData <- rData[,-c(1:5)]
  nData <- rData[,!grepl('Slc17a7',colnames(rData))]
  nData <- sweep(nData,1,apply(nData,1,sum),FUN='/')
  # do tsne, write to metadata
  set.seed(123)
  tsnes <- Rtsne(nData,perplexity=perplexity)
  mData$tsne1 <- tsnes$Y[,1]
  mData$tsne2 <- tsnes$Y[,2]
  # do umap, write to metadata
  umaps <- umap(nData)
  mData$umap1 <- umaps$layout[,1]
  mData$umap2 <- umaps$layout[,2]
  # do hierarchical clustering, write to metadata
  theClus <- hclust(dist(nData),method='ward.D2')
  print(plot(theClus))
  clustH <- cutree(theClus,k=nClust)
  mData$clust <- as.factor(clustH)
  # give global scope to rData,mData,nData (i.e. allow other functions
  # to retrieve these values
  rData <<- rData
  mData <<- mData
  nData <<- nData
}
