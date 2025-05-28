#' summarize authorships by gender, authorship position, and income
#'
#' @title summarize_gender_position_income
#' @param auths_with_gender authorship and pronoun data
#' @return
#' @author Cecilia Sanchez
#' @export
summarize_gender_position_income <- function(auths_with_gender){
  
  summary_table <- auths_with_gender %>% 
    group_by(authorship_position, gender_final, income_majority) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    mutate(perc = n/sum(n)*100)
  
  return(summary_table)
  
}
