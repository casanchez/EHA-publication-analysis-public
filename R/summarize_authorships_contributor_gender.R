#' summarize number of authorships by each contributor and their gender
#'
#' @title summarize_authorships_contributor_gender
#' @param auths_with_gender authorship and gender data
#' @return
#' @author Cecilia Sanchez
#' @export
summarize_authorships_contributor_gender <- function(auths_with_gender){
  
  summary_table <- auths_with_gender %>% 
    # keep only male and female for plotting purposes
    filter(gender_final == "gendered male" | gender_final == "gendered female") %>% 
    group_by(cleaned_name, gender_final) %>% 
    dplyr::summarise(n = n())
  
  return(summary_table)
  
}