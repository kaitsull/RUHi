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
  df <- mFISH@rawData
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
    mFISH@metaData <- dplyr::mutate(mFISH@metaData,
                                    fil = ifelse(id %in% mFISH@filteredData$id,
                                           'in', 'out'))
    mFISH@filteredData <- df
    mFISH@attributes$filter.by <- filter.by
    mFISH@attributes$threshold <- threshold
  }
  #return mFISH object
  mFISH
}
