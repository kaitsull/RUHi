#' Create a box plot for a given cluster
#'
#' @author Kaitlin E Sullivan
#'
#' @param mFISH An mFISH object
#' @param clus Cluster name or number in quotations (or "all" for every cluster)
#' @param metadata Metadata variable to plot (must be numeric)
#'
#' @return A box plot showing gene expression per cluster
#' @export
#'
#' @import ggplot2 dplyr tidyr
clusterBoxPlot <- function(mFISH, clus = "all", metadata = "genes"){
  #save data
  fdf <- m@filteredData
  md <- m@metaData
  md <- dplyr::filter(md, fil == T)

  #add md
  #add md
  l <- ncol(md)
  nm <- names(md)
  for(i in 1:l){
    cur <- nm[i]
    fdf <- dplyr::mutate(fdf, !!cur := md[,i])
  }

  df <- tidyr::pivot_longer(fdf, cols = -names(md),
                            names_to = "genes", values_to= "Expression")

  if(clus != "all"){
    df <- dplyr::filter(df, cluster==clus)
  }

  #plot
  gp <- ggplot(df, aes_string(x="genes", y="Expression", fill="genes"))+
    geom_boxplot(outlier.shape = NA)+
    theme_classic()+
    labs(y="Expression", x="")+
    facet_wrap(~cluster)


  gp
}
