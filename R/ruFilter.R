#' Select features and filter your mFISH object by a given gene
#'
#' @author Kaitlin E Sullivan
#'
#' @param mFISH An mFISH object
#' @param threshold A numeric value to threshold the gene by
#' @param filter.by A gene or a vector of gene names to filter data by
#' @param exclude A gene or vector of gene names to exclude from analysis
#'
#' @import dplyr rlang
#'
#' @return An mFISH object with a populated filteredData section
#'
#' @export
ruFilter <- function(mFISH, threshold = 0.1, filter.by = NA, exclude = NA){
  #save raw data
  df <- rawData(mFISH)
  #filter by list
  #warning if nothing to filter by
  if(is.na(filter.by) && is.na(exclude)){
    warning("Please select a gene or vector of genes to filter the data by or exclude from analysis!")
  }
  else{
    #change this to include a list of thresholds?????
    if(!is.na(filter.by)){
      l <- length(filter.by)
      #filter each gene and remove from dataframe
      for (i in 1:l) {
        print(paste("Filtering data by ", filter.by[i], " at threshold of ", threshold, "...", sep=""))
        #error if name does not exist
        if(!(filter.by[i] %in% names(df))){
          print(paste("WARNING: ", filter.by[i], " is not present in this current dataset. Please check spelling!",
                      sep=""))
        }
        df <- dplyr::filter(df, !!rlang::sym(filter.by[i])>threshold)
        #POTENTIAL ROADBLOCK???
        df <- dplyr::select(df, -(!!rlang::sym(filter.by[i])))
      }
    }
    if(!is.na(exclude)){
      print("Running gene exclusions...")
      l <- length(exclude)
      for(i in 1:l){
        if(!(exclude %in% names(df))){
          warning(paste("Gene ", exclude[i], " has already been removed via filtering or does not exist in this object. This argument is for removing genes that are not included in the filtering process. Please also check spelling!",
                        sep=""))
        }
        #POTENTIAL ROADBLOCK???
        df <- dplyr::select(df, -(!!rlang::sym(exclude[i])))
      }
    }
    #populate object with values
    #filtered
    filteredData(mFISH) <- df
    #attributes
    my.attrib <- getAttrib(mFISH)
    my.attrib$filter.by <- filter.by
    my.attrib$thresh <- threshold
    setAttrib(mFISH) <- my.attrib
    #mFISH@attributes$filter.by <- filter.by
    #mFISH@attributes$thresh <- threshold

    #save filtered ids
    fils <- dplyr::select(filteredData(mFISH), id)
    fils <- dplyr::mutate(fils, fil = T)
    others <- dplyr::filter(rawData(mFISH), !(id %in% fils$id))
    others <- dplyr::select(others, id)
    others <- dplyr::mutate(others, fil = F)


    #bind
    b <- rbind(fils, others)
    print("Updating metadata...")
    b <- dplyr::arrange(b, id)
    md <- metaData(mFISH)
    metaData(mFISH) <- dplyr::mutate(md, fil = b$fil)


  }
  #return mFISH object
  mFISH
}
