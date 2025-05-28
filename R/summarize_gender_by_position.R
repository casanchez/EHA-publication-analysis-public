#' summarize gender by authorship position (first and last)
#'
#' @title summarize_gender_by_position
#' @param auths_with_gender authorship and gender data
#' @return
#' @author Cecilia Sanchez
#' @export
summarize_gender_by_position <- function(auths_with_gender){
  
  n_auth <- nrow(auths_with_gender)
  
  summary_table <- auths_with_gender %>% 
    group_by(authorship_position, gender_final) %>% 
    summarize(n = n()) %>% 
    mutate(perc_of_all_FLA = n/n_auth*100)
  
  return(summary_table)
  
}
