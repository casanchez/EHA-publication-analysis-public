#' join contributor data to authorship data
#'
#' @title add_gender_to_auths_high_prod_filter
#' @param contributor_data contains gender data for unique contributors
#' @param authorship_data
#' @return
#' @author Cecilia Sanchez
#' @export
add_gender_to_auths_high_prod_filter <- function(contributor_data, 
                                                 authorship_data,
                                                 auth_count = 10){
  
  # join gender data and country economic development data to authorship data
  auths_with_gender <- authorship_data %>% 
    left_join(contributor_data[, c("cleaned_name", 
                                 "pronouns", 
                                 "gender_based_on_pronouns",
                                 "nqg_classification", 
                                 "p_gf",
                                 "gender_based_on_nqg",
                                 "method_match",
                                 "gender_final")]) %>% 
    mutate(gender_final = as.factor(gender_final))
  
  
  auths_with_gender_no_high_prod <- drop_hp_auths(auths_with_gender,auth_count)
  
  out <- list(auths_with_gender, auths_with_gender_no_high_prod)
  
  return(out)
  
}
