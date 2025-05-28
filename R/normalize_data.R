#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#'
#' @param df Data frame with column to be normalized
#' @param col string. name of column to be normalized
#'
#' @return
#' @author Collin Schwantes
#' @export
normalize_data <- function(df = g_fortified, col = "betweenness") {
  norm_func <- ecdf(x = df[,col])
  
  normalized <- norm_func(df[,col])
  new_col <- sprintf("%s_normalized",col)
  
  df[new_col]<-normalized*100
  return(df)
}
