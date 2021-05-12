#' Normalize and filter an mFISH dataset
#'
#' @param table Table made by the ruMake() function.
#' @param filter A gene name to filter data by before normalizing.
#' @param value A count-per-area value to be used in the filtering step
#' @return A normalized dataframe containing the quantified mFISH.
#'
#'
#' @export
ruNormal <- function(table, filter = NA, value = 0){
  print("This does nothing :)")
}
