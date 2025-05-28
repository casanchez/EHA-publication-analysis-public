#' clean publication data for further use
#'
#' @title clean_publication_data
#' @param publication_data_raw
#' @return
#' @author Cecilia Sanchez
#' @export
clean_publication_data <- function(publication_data_raw){
  
  publication_data <- publication_data_raw %>% 
    janitor::clean_names() %>% 
    mutate(publication_date = as.Date(publication_date)) %>% 
    mutate(country_of_study = gsub("\"", "", country_of_study)) %>% 
    # also change Hong Kong to China?
    mutate(country_of_study = gsub("Tibet", "China", country_of_study))
  
  # set up empty column
  publication_data$cc_of_study <- NA
  
  # get string of country codes for each paper
  for(i in 1:nrow(publication_data)){
    # Note that in this loop, "Non-specific" is converted to NA
    # there aren't any occurrences of "Namibia" in the dataset
    # but this would also produce "NA" as a country_code
    
    country_tbl <- as.vector(publication_data[i, "country_of_study"])
  
    country_vec <- unlist(strsplit(x = country_tbl$country_of_study, 
                                   split = ","))
    
    cc_of_study = toString(countrycode::countrycode(sourcevar = country_vec,
                                                    origin = "country.name", 
                                                    destination = "iso2c"))
    
    publication_data[i, "cc_of_study"] <- cc_of_study
    }
  
  return(publication_data)
}