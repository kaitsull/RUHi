#' Plot UMAP of mFISH data
#'
#' @param mFISH An mFISH object with UMAP coordinates
#' @param colour.by A metadata variable to colour by
#' @param size Point size
#' @param cols Vector of colours
#'
#' @return A ggplot showing UMAP space coloured by metadata.
#' @export
#'
#' @import ggplot2 dplyr
plotDim <- function(mFISH, colour.by="cluster", size = 0.9, cols=NA){

  #save umap object
  df <- mFISH@attributes$umap
  met <- dplyr::filter(mFISH@metaData, fil==T)
  uplot <- dplyr::mutate(met, UMAP_1 = df$UMAP_1, UMAP_2=df$UMAP_2)

  #baseplot
  p <- ggplot2::ggplot(uplot, aes_string(x="UMAP_1", y="UMAP_2", colour=colour.by))+
    theme_classic()+
    geom_point(size=size)

  #baseline colours
  if(class(colour.by)=='numeric'){
    p <- p + scale_colour_gradientn(values=c("cyan", "red"))
  }

  #others
  if(!is.na(cols)){
    if(class(colour.by)=='numeric'){
      p <- p + scale_colour_gradientn(values=cols)
    }
    else{
      p <- p + scale_colour_manual(values=cols)
    }
  }
  #print plot
  p
}
