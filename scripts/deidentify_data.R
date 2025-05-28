suppressPackageStartupMessages(source("packages.R"))

#' Hash items within a vector 
#'
#' @param x vector of any type. 
#'
#' @return hashed vector
#' @export
#'
#' @examples
row_level_hashing <- function(x){
  y <- purrr::map(x, rlang::hash)
  unlist(y)
}


row_level_date_obfuscation <- function(x){
  xsplit <- stringr::str_split(x,pattern = ", ")
  x_floor <- purrr::map_vec(xsplit, function(xsplit){
    # convert to date and get the greater of the two
    x_date <- max(lubridate::ymd(xsplit))
    
    # round the date down to the year
    x_date_floor <- lubridate::floor_date(x_date,unit = "year")
  })
  return(x_floor)
}


# contributor data
contributor_data_raw |> 
 names()

# cols to hash: 
#   name, record_id, institution_id_from_authorship, 
#   institution_display_name_from_institution_id_from_authorship, orcid,
#   source_ur_ls, google_scholar_id,screenshots, open_alex_id, ultimate_name
# we may want to consider dropping the publication identifiers 
# check on record_id


contributor_data_deident <- contributor_data_raw |>
  dplyr::mutate(name = row_level_hashing(name),
                research_outputs = row_level_hashing(research_outputs),
                identifier_from_research_outputs = row_level_hashing(identifier_from_research_outputs),
                institution_id_from_authorship = row_level_hashing(institution_id_from_authorship),
                institution_display_name_from_institution_id_from_authorship = row_level_hashing(institution_display_name_from_institution_id_from_authorship),
                cleaned_name = row_level_hashing(cleaned_name),
                orcid = row_level_hashing(orcid),
                source_ur_ls = row_level_hashing(source_ur_ls),
                google_scholar_id = row_level_hashing(google_scholar_id),
                screenshots = row_level_hashing(screenshots),
                open_alex_id = row_level_hashing(open_alex_id),
                ultimate_name = row_level_hashing(ultimate_name),
                screenshots_file_paths = row_level_hashing(screenshots_file_paths),
                reviewer = row_level_hashing(reviewer),
                recommended_staff_for_follow_up = row_level_hashing(recommended_staff_for_follow_up),
                publication_date = row_level_hashing(publication_date)
                )

readr::write_csv(x = contributor_data_deident,file = "data/deidentified_contributors.csv")

## authorship data

authorship_data_raw |> 
  names()


# cols to drop
# institution_id,name, cleaned_name



authorship_data_deident <- authorship_data_raw |>
  dplyr::mutate(institution_id = row_level_hashing(institution_id),
                name = row_level_hashing(name),
                publication = row_level_hashing(publication),
                publication_link = row_level_hashing(publication_link),
                cleaned_name = row_level_hashing(cleaned_name)) %>% 
  dplyr::mutate(publication_date_from_research_outputs = row_level_date_obfuscation(publication_date_from_research_outputs))


readr::write_csv(x = authorship_data_deident,file = "data/deidentified_authorship.csv")


# publication data
publication_data_raw |> 
  names()

publication_data_deident <- publication_data_raw |>
  dplyr::mutate(name = row_level_hashing(name),
                identifier = row_level_hashing(identifier),
                data_management_plan_id_from_project = row_level_hashing(data_management_plan_id_from_project)) |>
  dplyr::mutate(publication_date = row_level_date_obfuscation(publication_date))


readr::write_csv(x = publication_data_deident,file = "data/deidentified_publications.csv")




