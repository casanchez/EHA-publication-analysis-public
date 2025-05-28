#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contributor_data
#' @return
#' @contributor Collin Schwantes
#' @export
classify_names_nqg <- function(contributor_data, base, contributor_table,
                               threshold = 0.1) {

  # py_install("nomquamgender",pip = TRUE)
  nqg <- reticulate::import("nomquamgender")

  # create model object
  model <- nqg$NBGC()

  names(contributor_data)[2] <- "record_id"
  
  print("dropping columns in contributor data")
  cont_data_min <- contributor_data %>%
    select(id, cleaned_name)
  print("getting distinct names in contributor data")  
  fl_contributors_distinct <- cont_data_min %>%
    distinct(cleaned_name, .keep_all = FALSE)
  # classify names
  model$threshold <- threshold
  name_annotated <- model$annotate(names = fl_contributors_distinct$cleaned_name,
                                   as_df = TRUE)
  name_classification <- model$classify(names = fl_contributors_distinct$cleaned_name)
  fl_contributors_distinct$nqg_classification <- name_classification
  fl_contributors_distinct$p_gf <- name_annotated$`p(gf)`
  
  print("joining data")
  contributors_classified <- left_join(x = cont_data_min,
                                       y = fl_contributors_distinct,
                                       by = "cleaned_name") 
  
  print(names(contributors_classified))
  
  print("prepping data for upload")
  data_for_update <- contributors_classified %>% 
    select(-cleaned_name) %>% 
    filter(!is.nan(p_gf))

  update_log <- airtabler::air_update_data_frame(base = base,
                                   table_name = contributor_table,
                                   record_ids = data_for_update$id,
                                   records = data_for_update)
  
  nan_p_gf_names <- contributors_classified %>% 
    filter(is.nan(p_gf))
  
  output_list <- list(
    update_log,
    nan_p_gf_names
  )
  
  return(output_list)
  
  
}
