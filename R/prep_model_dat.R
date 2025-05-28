#' Prep data for model to predict proportion female
#'
#' @title prep_model_data
#' @param auths_with_gender
#' @return
#' @author Cecilia Sanchez
#' @export

prep_model_dat <- function(auths_with_gender){
  
  # set up some factors
  auths_with_gender <- auths_with_gender %>% 
    mutate(year = lubridate::year(publication_date)) %>% 
    mutate(income_majority = factor(income_majority, 
                                    levels = c("L", "LM", "UM", "H")),
           income_majority = forcats::fct_collapse(income_majority,
                                                   "Low- and middle-income" = c("L", "LM", "UM"),
                                                   "High-income" = "H"),
           authorship_position = factor(authorship_position))
  
  # calculate total authorships by year, authorship position, and income
  tots <- auths_with_gender %>% 
    group_by(year, authorship_position, income_majority, .drop = F) %>% 
    summarise(tot_authorships = n())
  
  # calculate total authorships by year, authorship position, income, and gender
  model_dat <- auths_with_gender %>% 
    group_by(year, authorship_position, income_majority, gender_final, 
             .drop = F) %>% 
    summarise(n = n()) %>% 
    # join to get the denominator data
    left_join(tots) %>% 
    filter(gender_final == "gendered female") %>% 
    mutate(perc_female = n/tot_authorships*100) %>% 
    # just to have prettier plotting
    mutate(authorship_position = fct_recode(authorship_position, 
                                            "First" = "first", 
                                            "Last" = "last"),
           year_centered = year - 2011) %>% 
    dplyr::select(year, year_centered, authorship_position, income_majority, 
                  n_female = n, tot_authorships, perc_female)
  
  return(model_dat)
  
}