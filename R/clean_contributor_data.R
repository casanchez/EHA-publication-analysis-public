#' clean contributor data for further use
#'
#' @title clean_contributor_data
#' @param contributor_data_raw
#' @return
#' @author Cecilia Sanchez
#' @export
clean_contributor_data <- function(contributor_data_raw){
  
  contributor_data <- contributor_data_raw %>% 
    janitor::clean_names() %>% 
    dplyr::rename(
      contributor_id = id_1,
      institution_display_name = institution_display_name_from_institution_id_from_authorship,
      institution_country_code = institution_country_code_from_institution_id_from_authorship,
      in_analysis_timeframe = in_analysis_timeframe_from_research_outputs_3,
      authorship_position = authorship_position_from_authorship,
      resource_type = resource_type_from_research_outputs) %>% 
    # contributor_data_raw contains more data than was used for this analysis
    # need to filter based on our criteria
    dplyr::filter(
      grepl("1", in_analysis_timeframe, fixed = T),
      grepl("first", authorship_position, fixed = T) | grepl("last", authorship_position, fixed = T),
      grepl("journal article", resource_type, fixed = T)) %>% 
    # to cross-compare gender classifications based on pronouns vs nqg outputs
    mutate(gender_based_on_nqg = case_when(
      nqg_classification == "gm" ~ "gendered male",
      nqg_classification == "gf" ~ "gendered female",
      nqg_classification == "-" ~ "uncertain",
      is.na(nqg_classification) ~ "undetermined")) %>% 
      mutate(method_match = ifelse(gender_based_on_pronouns == 
                                     gender_based_on_nqg, T, F)) %>% 
    # for cases where we were unable to classify gender based on pronouns, 
    # use gender classification based on nqg instead
    mutate(gender_final = case_when(
      gender_based_on_pronouns == "undetermined" ~ gender_based_on_nqg,
      TRUE ~ gender_based_on_pronouns)) %>% 
    mutate(gender_final = forcats::fct_collapse(
      gender_final,
      unknown = c("uncertain", "undetermined"),
      nonbinary = "non-binary/gender non-conforming"))
  
  return(contributor_data)
}