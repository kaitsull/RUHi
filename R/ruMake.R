#' Create an mFISH object
#'
#' @author Kaitlin E Sullivan
#'
#' @param df Dataframe from `ruRead()` or `ruCombine()`.
# @param filteredData Dataframe containing the filtered and normalized dataset with id number
#' @param metadata Dataframe containing metadata
# @param attributes List of attributes used in data analysis: `list(filter.by, thresh, umap_nn, umap_mindist, umap_metric, hclust_k, hclust_metric, hclust_p)`
#' @return An mFISH object.
#'
#' @import dplyr
#'
#' @export
ruMake <- function(df, metadata=NA) {
  print("Creating object...")
  #create mFISH object class
 mFISH <- setClass(Class='mFISH',
                  slots=c('rawData'='data.frame', 'filteredData'='list',
                          'metaData'='data.frame', 'attributes'='list'))
  #get class of input
  isDf <- class(df)
  #check for these values in names
  checkVal <- c("section", "anum", "region")

  #if data.frame - start from scratch
  if(isDf=='data.frame'){
    df <- dplyr::select(ruCombine, -c(X,Y))
    df <- dplyr::mutate(ruCombine, )
    meta <- dplyr::select(ruCombine, c(X,Y))

    #allocate meta data and raw data to respective slots
    otherData  <- checkVal %in% names(ruCombine)
    for(i in 1:length(checkVal)){
      if(otherData[i]==T){
        df <- dplyr::select(df, -checkVal[i])
        meta <- dplyr::select(meta, checkVal[i])
      }
    }

    #include other metadata
    if(!is.na(metadata)){
      for(i in 1:length(names(metadata))){
        cur <- names(metadata[i])
        meta$cur <- metadata$cur
      }
    }

    #enter rawData and metaData
    #leave filterData and attributes for future functions
    hiPlex <- mFISH(rawData = df, filterData = NA,
                    metaData = meta,
                    attributes = list(filter.by=NA, thresh=NA, umap_nn=NA,
                                  umap_mindist=NA, umap_metric=NA, hclust_k=NA,
                                  hclust_metric=NA, hclust_p=NA, pca=NA, npc=NA))
    #return object
    hiPlex
  }
  else{
    #print error if not a dataframe
    print("Incorrect class. ruCombine should be a dataframe output from the function ruCombine().")
  }
}
