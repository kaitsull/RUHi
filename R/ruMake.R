#' Create a dataframe containing quantified mFISH data
#'
#' @param dir Directory containing the quantified channel tables.
#' @param region A character string with the name of the tissue region imaged.
#' @param id Animal number or unique identifier for tissue sample.
#' @return A dataframe containing the quantified mFISH data of all channels present.
#'
#'
#'
#' @export
ruMake <- function(dir, region = NA, id = NA) {
  i<-1
  filelist = list.files(pattern = "_quantification.csv$")
  print('The following files will be quantified:')
  print(filelist)

  #initialize hiplex dataframe with cell # and X, Y position
  geneData <- read_csv(filelist[i])
  hiPlex <- select(geneData, X, Y, X1)

  #linear or non linear
  if(grep("nl.tif",filelist[1])>0){
    del <- 44
  }
  else{
    del <- 38
  }

  while(i<(length(filelist)+1)){
    #extract gene name from file name
    cur <- filelist[i]
    geneName <- substr(cur, 8, (nchar(cur)-del))
    geneData <- read_csv(cur)

    #User-selected filtering of real vs unreal signal
    hiPlex <- mutate(hiPlex, !!geneName := (geneData$Mean/geneData$Area)*255)
    i<-i+1
  }
  if(is.na(region) | is.na(id)){
    title = "HiPlex.csv"
  }
  else{
    title = paste(region, "_", id, ".csv", sep = "")
  }
  #write .csv
  write.csv(hiPlex,'HiPlex_QuantificationTable.csv')
  #return dataframe
  hiPlex
}
