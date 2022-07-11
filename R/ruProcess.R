#' Normalizes data and runs a PCA
#'
#' @author Kaitlin E Sullivan
#'
#' @param mFISH An mFISH object containing raw or filtered data
#' @param remove.outliers A logical value for removing "outliers" (eg: autofluorescent cells positive for all genes)
#' @param outlier.thresh Low and high threshold for outliers. These numbers will change based on amount of genes and autofluorescence in your data.
#'
#' @return and mFISH object, normalized and containing a PCA (found in `obj@attributes$pca`)
#'
#' @import dplyr
#'
#' @export
#'
ruProcess <- function(mFISH, remove.outliers=F, outlier.thresh=c(1,11)){
  #ensure the class of the object
  if(class(mFISH) != 'mFISH'){
    warning("This function only takes objects of class `mFISH`. You can generate this type of object using `ruMake()` or `ruCombine()`.")
  }

  #check for filtered data
  if(is.na(mFISH@filteredData)){
    mFISH@filteredData <- mFISH@rawData
  }

  #normalize the data
  df <- mFISH@filteredData
  #exclude id values from norm/pca
  id <- dplyr::select(df, id)
  df <- dplyr::select(df, -id)

  #remove 0 value
  if(remove.outliers){
    print("Removing outliers...")
    #single genes
    n<-dplyr::mutate(df, nfts =rowSums(n>0))
    id <- dplyr::mutate(id, nfts=n$nfts)
    n<-dplyr::filter(df, nfts>outlier.thresh[1])
    n<-dplyr::filter(df, nfts<outlier.thresh[2])
    id<-dplyr::filter(id, nfts %in% df$nfts)
    n<-dplyr::select(n, -nfts)
  }

  print("Normalizing data...")
  df <- sweep(df, 1,apply(df, 1, sum), "/")
  df <- df*(100)

  print("Running PCA...")
  #run a pca
  pca <- prcomp(df, center = T, scale = T)

  #save the data
  mFISH@filteredData <- dplyr::mutate(df, id=id)
  mFISH@attributes$pca <- pca$x

  #return object
  mFISH
}
