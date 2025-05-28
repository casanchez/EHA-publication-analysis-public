#' Creates fortified igraph object for plotting and adds simplified country
#' labels
#'
#' @title
#' @param g_contrib_gender_btw_deg_hc
#' @return
#' @author Collin Schwantes
#' @export
make_fortified_igraph_obj <- function(g_contrib_gender_btw_deg_hc) {

  g_fortified <- ggnetwork::fortify(g_contrib_gender_btw_deg_hc)
  
  country_labels <- g_fortified %>% 
    group_by(primary_country) %>% 
    tally() %>% 
    mutate(country_label = case_when(
      n < 20 ~ "Other",
      TRUE ~ primary_country
    ))
  
  g_fort_simple_country <- left_join(g_fortified,country_labels,"primary_country")
  
  return(g_fort_simple_country)
  
}
