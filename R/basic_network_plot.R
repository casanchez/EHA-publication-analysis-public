#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param g_fortified data frame. Fortified igraph object
#' @param nodes_color character. Node property to use as color
#' @return
#' @author Collin Schwantes
#' @export
basic_network_plot <- function(g_fortified, nodes_color = "gender") {

  ggplot(g_fortified, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges( aes(alpha = weight),color = "black") +
    geom_nodes(aes(color = .data[[nodes_color]])) +
    ggnetwork::theme_blank() +
    scale_alpha_continuous(range = c(0.15,1))
}
