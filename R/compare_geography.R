#' compare focal country of paper with first and last author geographic affiliations
#'
#' @title compare_geography
#' @param publication_data
#' @param authorship_data
#' @return
#' @author Cecilia Sanchez
#' @export
compare_geography <- function(publication_data, authorship_data){
  
  # restrict to only papers that were about a given country/countries
  pubs_with_focal_country <- publication_data %>% 
    #mutate(country_of_study = gsub("\"", "", country_of_study)) %>% 
    filter(country_of_study != "Non-specific")
  
  # set up empty columns
  pubs_with_focal_country$first_auth_cc <- NA
  pubs_with_focal_country$last_auth_cc <- NA
  
  # for a given publication:
  for(i in pubs_with_focal_country$identifier){
    
    # get the country code of first author
    first_auth_cc <- authorship_data %>% 
      filter(publication_link == i,
             authorship_position == "first") %>% 
      select(iso2) %>% 
      distinct() %>% 
      pull()
    
    # save country code of first author
    pubs_with_focal_country$first_auth_cc[pubs_with_focal_country$identifier == i] <- first_auth_cc
    
    # get the country code of last author
    last_auth_cc <- authorship_data %>% 
      filter(publication_link == i,
             authorship_position == "last") %>% 
      select(iso2) %>% 
      distinct() %>% 
      pull()
    
    # save country code of last author (if there was a last author)
    if(!is_empty(last_auth_cc)){
      pubs_with_focal_country$last_auth_cc[pubs_with_focal_country$identifier == i] <- last_auth_cc
    }
    
  }
  
  geo_matches <- pubs_with_focal_country %>% 
    mutate(
      first_geo_match = case_when(
        str_detect(string = cc_of_study, pattern = first_auth_cc) ~ "yes",
        is.na(first_auth_cc) ~ NA,
        .default = "no"),
      last_geo_match = case_when(
        str_detect(string = cc_of_study, pattern = last_auth_cc) ~ "yes",
        is.na(last_auth_cc) ~ NA,
        .default = "no"),
      either_geo_match = case_when(
        first_geo_match == "yes" | last_geo_match == "yes" ~ "yes",
        .default = "no"),
      both_geo_match = case_when(
        first_geo_match == "yes" & last_geo_match == "yes" ~ "yes",
        # can only have both authors match when the paper had first and last authors
        is.na(last_geo_match) ~ NA,
        .default = "no")
    )
  
  publication_data_with_geo <- left_join(publication_data, geo_matches) %>% 
    mutate(cc_of_study = na_if(cc_of_study, "NA"))
  
  return(publication_data_with_geo)
  
}
