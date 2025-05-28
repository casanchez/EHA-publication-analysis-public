#' summarize the authorship items by the institution country code
#'
#' @title summarize_by_country
#' @param authorship_data
#' @param income_data_clean
#' @return
#' @author Collin Schwantes
#' @export
summarize_by_country <- function(authorship_data, income_data_clean) {

   ### Break down by country/geographic region
  country_summary <- authorship_data %>% 
    group_by(iso2) %>% 
    count() %>% 
    ungroup()
  
  ### get country codes and GNGS status 
  country_summary_coded <- left_join(country_summary,
                                     countrycode::codelist,
                                     by = c("iso2" = "iso2c"),
                                     na_matches = "never") %>% 
    
    left_join(income_data_clean[, c("iso3", "income_majority")],
              countrycode::codelist,
              by = c("iso3c" = "iso3"))
    
  out <- country_summary_coded %>% 
    dplyr::select(iso2, n, country.name.en, continent, region, income_majority) 
  
  return(out)

}
