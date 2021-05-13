#' Create a dataframe with raw pixel info
#'
#' @return A dataframe containing the raw pixels mFISH data of all channels present.
#'
#'
#'
#' @export
rawPixels <- function() {
  i<-1
  filelist = list.files(pattern = "_quantification.csv$")
  print('The following files will be quantified:')
  print(filelist)

  #initialize hiplex dataframe with cell # and X, Y position
  geneData <- read.csv(filelist[i])
  hiPlex <- select(geneData, X, Y, Area)

  #linear or non linear
  if(length(grep("nl.tif",filelist[1]))==0){
    del <- 38
  }
  else if(length(grep("nonlinear",filelist[1])==0)){
    del <- 77
  }
  else{
    del <- 43
  }

  while(i<(length(filelist)+1)){
    #extract gene name from file name
    cur <- filelist[i]
    geneName <- substr(cur, 8, (nchar(cur)-del))
    geneData <- read.csv(cur)

    #User-selected filtering of real vs unreal signal
    hiPlex <- mutate(hiPlex, !!geneName := (geneData$Mean/255)*geneData$Area)
    i<-i+1
  }
  if(is.na(region) | is.na(id)){
    title = "HiPlex.csv"
  }
  else{
    title = paste(region, "_", id, ".csv", sep = "")
  }
  #write .csv
  write.csv(hiPlex,'pixtable.csv')
  #return dataframe
  hiPlex
}
