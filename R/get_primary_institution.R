#' Pulls the most frequently listed affiliation for an author
#'
#' We are interested in where an author has published the bulk of their work,
#' not where they are currently working.
#'
#' @param df_contrib data frame. Reduced contributor dataframe
#'
#' @return Data frame with primary affiliation. 
#' @author Collin Schwantes
#' @export
get_primary_affiliations <- function(df_contrib) {
  
  x <- df_contrib$institution_display_name %>% 
    purrr::map_chr(pull_most_frequent_element)
  
  df_contrib$primary_affiliation <- x
  
  return(df_contrib)
  
}

get_primary_country <- function(df_contrib){
  
  x <- df_contrib$institution_country_code %>% 
    purrr::map_chr(pull_most_frequent_element)
  
  df_contrib$primary_country <- x
  
  return(df_contrib)
}

#' Pull the most frequent element from comma separated items
#'
#' @param x Character. String with comma separated elements
#'
#' @return Character vector with most frequent value.
#' @export
#'
#' @examples
#' 
#' x <- c("hello, world, world")
#' pull_most_frequent_element(x) # returns world
#' 
#' x2 <- c("hello, world", "good night, moon, moon")
#' pull_most_frequent_element(x2)
#' # returns hello and moon
#' 
#' x3 <- c("zoo, book")
#' pull_most_frequent_element(x3)
#' # returns book
#' 
pull_most_frequent_element <- function(x){
  
  affiliations <- str_split(x,", ")
  affil_freq <- table(affiliations)
  y_df <- as.data.frame(affil_freq)
  
  primary_affiliation <-  y_df %>% 
    arrange(desc(Freq)) %>% 
    slice(1) %>% 
    pull(affiliations) %>% 
    as.character()
  
  return(primary_affiliation)
  
}
