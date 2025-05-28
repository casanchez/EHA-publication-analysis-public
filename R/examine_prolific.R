#'  # look at % of last authorships for most prolific authors
#'
#' @title examine_prolific
#' @param auths_with_gender authorship and gender data
#' @param prolific_cutoff authors with >= this many authorships are considered "prolific"
#' @return
#' @author Cecilia Sanchez
#' @export
examine_prolific <- function(auths_with_gender, prolific_cutoff){
  
  auths_with_gender <- auths_with_gender %>% 
    filter(gender_final == "gendered male" | gender_final == "gendered female")
  
  # authorships by name, gender, position
  t1 <- auths_with_gender %>% 
    group_by(cleaned_name, gender_final, authorship_position) %>% 
    dplyr::summarise(n = n())
  
  # authorships by name, gender
  t2 <- auths_with_gender %>% 
    group_by(cleaned_name, gender_final) %>% 
    dplyr::summarise(tot_FLA = n())
  
  # look at % of last authorships for most prolific authors
  t3 <- t1 %>% 
    left_join(t2) %>% 
    arrange(desc(tot_FLA)) %>% 
    dplyr::filter(tot_FLA >= prolific_cutoff,
                  authorship_position == "last") %>% 
    mutate(perc_last_auth = round(n/tot_FLA*100, 1))
  
  return(t3)
  
}
