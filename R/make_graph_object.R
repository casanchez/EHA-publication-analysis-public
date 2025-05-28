#' Make the graph object to analyzed
#' 
#' Creates a bipartite graph between publications and authors then 
#' reprojects the graph to show connections between authors based on
#' co-publications. Additional attributes are added to the author nodes
#' from the contributor data frame.
#'
#' @title
#' @param df_auth data frame. Contains authorship data
#' @param df_contrib data frame. Contains contributor data
#' @return
#' @author Collin Schwantes
#' @export
make_graph_object <- function(df_auth, df_contrib) {
  
  tdf <- table(df_auth)
  g_bi <- igraph::graph_from_biadjacency_matrix(tdf, weighted = TRUE)
  
  g_bi_bp <- igraph::bipartite_projection(g_bi)
  
  g_contrib <- g_bi_bp$proj1
  
  df_vertex <- data.frame(contributor_id = V(g_contrib)$name)
  
  ## for some reason NA's introduced for genders... 
  df_vertex_gender <- left_join(df_vertex,df_contrib)
  
  
  g_contrib_gender <- igraph::set_vertex_attr(g_contrib,name = "gender",value = df_vertex_gender$gender_final)
  g_contrib_gender <- igraph::set_vertex_attr(g_contrib_gender,name = "primary_affiliation",value = df_vertex_gender$primary_affiliation)
  g_contrib_gender <- igraph::set_vertex_attr(g_contrib_gender,name = "primary_country",value = df_vertex_gender$primary_country)
  
  return(g_contrib_gender)
}
