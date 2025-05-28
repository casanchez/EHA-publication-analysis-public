#' Pulls nodes with degree greater than or equal to degree_floor
#'
#' @param degree_floor numeric. Minimum value for degree 
#' @param g_contrib_gender igraph object 
#'
#' @return
#' @author Collin Schwantes
#' @export
get_highly_connected_individuals <- function(g_contrib_gender, degree_floor = 2) {

  graph_degree <- igraph::degree(graph = g_contrib_gender,v = V(g_contrib_gender),loops = FALSE)
  
  highly_connected_individuals <- graph_degree[graph_degree >= degree_floor]
  
  return(highly_connected_individuals)
}
