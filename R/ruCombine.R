#' Create a combined dataframe containing quantified mFISH data
#'
#' @author Kaitlin E Sullivan
#'
#' @param dfs A vector of dataframes (from `ruRead()`)
#' @return A combined dataframe.
#'
#' @import dplyr
#'
#' @export
ruCombine <- function(dfs) {
  #if the data is 2+ dataframes
  if(length(dfs)>1){
    print("Combining tables...")

    #save the last id number of first df
    cur <- objs[1]
    lst <- nrow(cur)

    #iterate through
    for(i in 1:length(objs)-1){
      print(paste("Table: ", i+1, sep=""))
      #warning if cannot combine tables
      if(!(names(cur) %in% names(objs[i+1]))){
        warning(paste("Column names of tables ", i, " and ", i+1, "are not the same.", sep=""))
      }

      #change id numbers for subsequent round
      objs[i+1] <- mutate(objs[i+1], id=id+lst)
      #bind tables
      cur <- rbind(cur, objs[i+1])
    }
    #return table
    cur
  }
  else{
    warning("This function requires a vector of dataframes with length >= 2.")
  }
}
