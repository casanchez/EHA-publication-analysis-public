#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param obs_table
#' @param auths_with_gender
#' @return
#' @author Collin Schwantes
#' @export
make_leadership_gender_table <- function(obs_table, auths_with_gender) {

  
  obs_table <- obs_table |>
    dplyr::mutate(contributor_id = stringr::str_remove_all(contributor_id,pattern = "\"")) |>
    dplyr::mutate(snapshot_date = stringr::str_remove_all(snapshot_date,pattern = "\""))

  obs_table_minimal <- obs_table |>
    dplyr::mutate(snapshot_date = lubridate::ymd(snapshot_date)) |>
    dplyr::mutate(leadership_role = as.logical(leadership_role)) |>
    dplyr::select(contributor_id, snapshot_date,leadership_role) |>
    dplyr::filter(contributor_id != "")
  
  contrib_gender <- auths_with_gender |>
    dplyr::select(gender_final,contributor_id) |>
    dplyr::distinct(contributor_id,.keep_all = TRUE)
  
  
  leadership_gender <- dplyr::left_join(x = obs_table_minimal,y = contrib_gender,by = "contributor_id") |>
    dplyr::mutate(snap_year = lubridate::year(snapshot_date))

  return(leadership_gender)
}
