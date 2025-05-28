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
# pubs_with_focal_country$first_auth_cc <- NA
# pubs_with_focal_country$last_auth_cc <- NA
pubs_with_focal_country$first_geo_match <- NA
pubs_with_focal_country$last_geo_match <- NA
pubs_with_focal_country$either_geo_match <- NA

# for a given publication:
for(i in pubs_with_focal_country$identifier){
  
  # get the country code of first author
  first_auth_cc <- authorship_data %>% 
    filter(publication_link == i,
           authorship_position == "first") %>% 
    select(iso2) %>% 
    distinct() %>% 
    pull()
  
  # get the country code of last author
  last_auth_cc <- authorship_data %>% 
    filter(publication_link == i,
           authorship_position == "last") %>% 
    select(iso2) %>% 
    distinct() %>% 
    pull()
  
  # get the country codes of the focal country/ies
  paper_cc <- pubs_with_focal_country %>% 
    filter(identifier == i) %>% 
    pull(cc_of_study)
  
  # test if first author cc matches with any of the focal countries 
  first_match <- str_detect(string = paper_cc, pattern = first_auth_cc)
  # store the result
  pubs_with_focal_country$first_geo_match[pubs_with_focal_country$identifier == i] <- first_match
  
  # was there a last author?
  if(is_empty(last_auth_cc)){
    # if no, then we can't evaluate a match
    last_match <- NA
  } else{
    # if there is, test if last author cc matches with any of the focal countries 
    last_match <- str_detect(string = paper_cc, pattern = last_auth_cc)
    }
  # store the result
  pubs_with_focal_country$last_geo_match[pubs_with_focal_country$identifier == i] <- last_match

  # note if either author matched
  pubs_with_focal_country$either_geo_match[pubs_with_focal_country$identifier == i] <- sum(first_match, last_match, na.rm = T)
}

publication_data_with_geo <- left_join(publication_data, pubs_with_focal_country)

return(publication_data_with_geo)

}
