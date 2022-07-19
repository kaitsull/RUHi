#' Create and write a dataframe containing quantified mFISH data
#'
#' @param dir Directory containing the quantified channel tables.
#' @param region A character string with the name of the tissue region imaged.
#' @param anum Animal number or unique identifier for tissue sample.
#' @param section Section number
#' @return A dataframe containing the quantified mFISH data of all channels present.
#'
#'
#' @export
#'
#' @import utils dplyr
ruRead <- function(dir, region = NA, anum = NA, section=NA) {
  i<-1
  filelist = list.files(path = dir, pattern = "_quantification.csv$")
  print('The following files will be quantified:')
  print(filelist)

  #initialize hiplex dataframe with cell # and X, Y position
  geneData <- utils::read.csv(paste(dir, .Platform$file.sep, filelist[i], sep = ""))
  hiPlex <- select(geneData, X, Y)

  #linear or non linear
  if(grepl("_NL.tif", filelist[1])){
    #current code name - assumes NL too
    del <- 45
  }
  else if(grepl("rigid", filelist[1])){
    #long form name (old code)
    del <- 62
  }
  else{
    #3-4plex, no registration
    del <- 23
  }



  while(i<(length(filelist)+1)){
    #extract gene name from file name
    cur <- filelist[i]
    geneName <- substr(cur, 8, (nchar(cur)-del))
    geneData <- utils::read.csv(cur)

    #User-selected filtering of real vs unreal signal
    hiPlex <- dplyr::mutate(hiPlex, !!geneName := (geneData$Mean/geneData$Area)*255)
    i<-i+1
  }

  #name
  title <- "mFISH"
  if(!is.na(region)){
    title <- paste(title, region, sep="_")
    hiPlex <- dplyr::mutate(region=region)
  }
  else if(!is.na(anum)){
    title <- paste(title, anum, sep="_")
    hiPlex <- dplyr::mutate(anum=anum)
  }
  else if(!is.na(section)){
    title <- paste(title, section, sep="_")
    hiPlex <- dplyr::mutate(section=section)
  }

  #save title and id
  title <- paste(title, ".csv", sep = "")
  hiPlex <- dplyr::mutate(hiPlex, id=1:nrow(hiPlex))

  #write .csv
  utils::write.csv(hiPlex, title)
  #return dataframe
  hiPlex
}
