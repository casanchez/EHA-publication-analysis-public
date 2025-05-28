#' clean authorship data for further use
#'
#' @title clean_authorship_data
#' @param authorship_data_raw
#' @param income_data_clean
#' @return
#' @author Cecilia Sanchez
#' @export
clean_authorship_data <- function(authorship_data_raw, income_data_clean){
  
  authorship_data <- authorship_data_raw %>% 
    janitor::clean_names() %>% 
    rename(iso2 = institution_country_code_from_institution_id,
           publication_date = publication_date_from_research_outputs,
           resource_type = resource_type_from_publication,
           within_analysis_timeframe = within_analysis_timeframe_from_publication) %>% 
    mutate(resource_type = str_remove_all(resource_type, '"')) %>% 
    # authorship_data_raw contains more data than was used for this analysis
    # need to filter based on our criteria
    dplyr::filter(
      grepl("1", within_analysis_timeframe, fixed = T),
      grepl("first", authorship_position, fixed = T) | grepl("last", authorship_position, fixed = T),
      grepl("journal article", resource_type, fixed = T)) %>% 
    mutate(country_name = countrycode::countrycode(iso2, "iso2c", 
                                                   "country.name"),
           iso3 = countrycode::countrycode(iso2, "iso2c", 
                                                   "iso3c"),
           publication_date = as.Date(publication_date),
           publication = gsub("\"", "", publication)) %>%
    
    left_join(income_data_clean[, c("iso3", "income_majority")])
  
  return(authorship_data)
}