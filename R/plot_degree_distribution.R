#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param g_contrib_gender
#' @return
#' @author collinschwantes
#' @export
plot_degree_distribution <- function(g_contrib_gender) {

  deg_dist <- igraph::degree_distribution(g_contrib_gender)
  
  df <- data.frame("degree" = 0:(length(deg_dist)-1),"freq" = deg_dist)
  ggplot2::ggplot(data = df, aes(x = degree, y = deg_dist)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw()
  
}
