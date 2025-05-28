#' Two panel plot to show authorship by gender, authorship by income group
#'
#' @title plot_authorship_two_panel
#' @param fig1a_plot
#' @param fig1b_plot
#' @return
#' @author Cecilia Sanchez
#' @export

plot_authorship_two_panel <- function(fig1a_plot, fig1b_plot){
  
  cowplot::plot_grid(fig1a_plot, fig1b_plot, labels = c('A', 'B'), 
                     label_size = 14)
  
}
