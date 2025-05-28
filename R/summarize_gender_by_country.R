#' summarize gender by country
#'
#' @title summarize_gender_by_country
#' @param auths_with_gender authorship and gender data
#' @param income_data_clean country income groups
#' @return
#' @author Cecilia Sanchez
#' @export
summarize_gender_by_country <- function(auths_with_gender, income_data_clean){
  
  # count the number of authorships by gender and country
  summary_table <- auths_with_gender %>% 
    group_by(gender_final, country_name, iso3) %>% 
    summarize(n = n()) %>% 
    left_join(income_data_clean[, c("iso3", "income_majority")])
  
  return(summary_table)
  
}