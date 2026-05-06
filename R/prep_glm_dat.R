#' Get total authorships by year, contributor, and gender
#' 
#' The idea here 
#' 
#' @title
#' @param auths_with_gender
#' @return
#' @author collinschwantes
#' @export
prep_glm_dat <- function( auths_with_gender) {

  # get year of publication
  auths_with_gender <- auths_with_gender %>% 
    mutate(year = lubridate::year(publication_date))
  
  # calculate total authorships by year, contributor, and gender
  out <- auths_with_gender %>% 
    # dropping groups that don't appear in the data.
    # we only count observed publications with EHA affiliations.
    group_by(cleaned_name,gender_final,year, .drop = TRUE) %>% 
    summarise(n = n()) |>
    dplyr::ungroup() |>
    dplyr::filter(stringr::str_detect(gender_final, "nonbinary|unknown",negate = T))

  
  return(out)

}
