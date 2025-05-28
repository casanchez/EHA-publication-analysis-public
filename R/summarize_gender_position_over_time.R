#' summarize authorships over time, by gender and authorship position
#'
#' @title summarize_gender_position_over_time
#' @param auths_with_gender authorship and pronoun data
#' @return
#' @author Cecilia Sanchez
#' @export
summarize_gender_position_over_time <- function(auths_with_gender){
  
  # summarize authorships by gender and authorship position over time
  summary_table <- auths_with_gender %>% 
    mutate(year = lubridate::year(publication_date)) %>% 
    mutate(year = factor(year), 
           authorship_position = factor(authorship_position)) %>% 
    group_by(gender_final, year, authorship_position, .drop = FALSE) %>% 
    summarize(n = n())
  
  return(summary_table)
  
}