#' Plot spatial location of cells coloured by metadata
#'
#' @param mFISH An mFISH object
#' @param colour.by A metadata variable to colour by
#' @param size Point size
#' @param cols Vector of colours
#' @param include.fil Logical allowing for filtered cells to appear in grey
#' @param facet A variable to separate plots by (eg: section or anum to view individual slices)
#'
#' @return A ggplot showing cells in native space!
#' @export
#'
#' @import ggplot2 dplyr
plotSpace <- function(mFISH, colour.by="cluster",
                      size = 0.9, cols=NA, include.fil=T, facet=NA){

  #save object
  df <- mFISH@rawData
  md <- mFISH@metaData
  df <- merge(df, md)

  fil.df <- dplyr::filter(df, fil==T)



  #baseplot with bg cells
  if(include.fil){
    p <- ggplot2::ggplot(fil.df, aes_string(x="X", y="Y", colour=colour.by))+
      theme_classic()+
      geom_point(data = df, colour='grey', size=size)+
      geom_point(size=size)
  }
  #without
  else{
    p <- ggplot2::ggplot(fil.df, aes_string(x="X", y="Y", colour=colour.by))+
      theme_classic()+
      geom_point(size=size)
  }

  #baseline colours
  if(class(df[[colour.by]])=='numeric'){
    p <- p + scale_colour_gradientn(colours=c("cyan", "red"))
  }

  #others
  if(!is.na(cols)){
    if(class(df[[colour.by]])=='numeric'){
      p <- p + scale_colour_gradientn(colours=cols)
    }
    else{
      p <- p + scale_colour_manual(values=cols)
    }
  }

  #facetwrap
  if(!is.na(facet)){
    p <- p + facet_wrap(as.formula(paste("~", facet, sep = "")))
  }

  #print plot
  p
}
