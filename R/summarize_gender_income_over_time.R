#' summarize authorships over time, by gender and income
#'
#' @title summarize_gender_income_over_time
#' @param auths_with_gender authorship and pronoun data
#' @return
#' @author Cecilia Sanchez
#' @export
summarize_gender_income_over_time <- function(auths_with_gender){
  
  summary_table <- auths_with_gender %>% 
    mutate(year = lubridate::year(publication_date)) %>% 
    mutate(year = factor(year), 
           income_majority = factor(income_majority)) %>% 
    group_by(gender_final, year, income_majority, .drop = FALSE) %>% 
    summarize(n = n())
  
  return(summary_table)
  
}