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
#' @importFrom stats prcomp na.omit
#'
#' @export
#'
ruProcess <- function(mFISH, remove.outliers=F, outlier.thresh=c(1,11)){
  #ensure the class of the object
  if(class(mFISH) != 'mFISH'){
    warning("This function only takes objects of class `mFISH`. You can generate this type of object using `ruMake()` or `ruCombine()`.")
  }

  #check for filtered data
  if(length(mFISH@filteredData)==0){
    mFISH@filteredData <- mFISH@rawData
  }

  #normalize the data
  df <- mFISH@filteredData

  #exclude id values from norm/pca
  ids <- dplyr::select(df, id)
  df <- dplyr::select(df, -id)

  #remove 0 value
  if(remove.outliers){
    print("Removing outliers...")
    #find cells with number of features outside of threshold
    df<-dplyr::mutate(df, nfts =rowSums(df>0))
    df <- dplyr::mutate(df, id=ids$id)
    ids <- dplyr::mutate(ids, nfts=df$nfts)

    #low/high
    df<-dplyr::filter(df, nfts>outlier.thresh[1])
    df<-dplyr::filter(df, nfts<outlier.thresh[2])

    #filter ids
    ids<-dplyr::filter(ids, id %in% df$id)

    #remove from table
    df<-dplyr::select(df, -c(nfts, id))
  }

  #normalize to PAC
  print("Normalizing data...")
  df <- sweep(df, 1,apply(df, 1, sum), "/")
  df <- df*(100)

  #remove NAs
  df <- dplyr::mutate(df, id = ids$id)
  df <- stats::na.omit(df)
  ids <- dplyr::filter(ids, id %in% df$id)

  print("Running PCA...")
  #run a pca
  df <- dplyr::select(df, -id)
  pca <- stats::prcomp(df, center = T, scale = T)

  #save the data
  mFISH@filteredData <- dplyr::mutate(df, id=ids$id)
  mFISH@attributes$pca <- pca$x

  #update filtered ids
  fils <- dplyr::select(mFISH@filteredData, id)
  fils <- dplyr::mutate(fils, fil = T)
  others <- dplyr::filter(mFISH@rawData, !(id %in% fils$id))
  others <- dplyr::select(others, id)
  others <- dplyr::mutate(others, fil = F)


  #bind
  print("Updating metadata...")
  b <- rbind(fils, others)
  b <- dplyr::arrange(b, id)
  mFISH@metaData <- dplyr::mutate(mFISH@metaData, fil = b$fil)


  #save w ids
  df <- dplyr::mutate(df, id = ids$id)
  mFISH@filteredData <- df

  #return object
  mFISH
}
