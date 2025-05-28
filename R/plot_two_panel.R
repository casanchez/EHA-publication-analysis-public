#' Two panel plot
#'
#' @title plot_two_panel
#' @param plot1 a plot that will be panel A
#' @param plot2 a plot that will be panel B
#' @return
#' @author Cecilia Sanchez
#' @export
plot_two_panel <- function(plot1, plot2, ncols, nrows, alignment, align_axis){
  
  cowplot::plot_grid(plot1, plot2, labels = c('A', 'B'), 
                     ncol = ncols, nrow = nrows, 
                     align = alignment, axis = align_axis,
                     label_size = 10)
}
