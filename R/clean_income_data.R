#' clean World Bank income data
#'
#' @title clean_income_data
#' @param income_data_raw
#' @return
#' @author Cecilia Sanchez
#' @export

clean_income_data <- function(income_data_raw){
  
    income_data_clean <- income_data_raw %>% 
    janitor::clean_names() %>% 
    dplyr::select(1:2, 27:38) %>% 
    filter(!row_number() %in% c(1:10, 229:239))
    
    names(income_data_clean) <- c("iso3", "country_name", 
                                  paste0("year_", 2011:2022))
    
    # income group can change from year to year
    # we'll use the majority value for each country across years
    income_data_clean$income_majority <- unlist(
      apply(income_data_clean[, 3:14], 
            1, 
            function(x) names(which.max(table(x))))
      )
    
    return(income_data_clean)
}