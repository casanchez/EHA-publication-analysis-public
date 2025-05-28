#' 
#'
#' @title
#' @param leadership_gender
#' @param auths_ioi_4
#' @return
#' @author Collin Schwantes
#' @export
summarize_publication_leadership <- function(leadership_gender, auths_ioi_4,toe_summary) {

  ## people who have toe on either side of 2019
  survived_2019 <- toe_summary |>
    dplyr::filter(contains_2019) |>
    dplyr::pull(contributor_id)
  

   fill_2019<- leadership_gender |>
    dplyr::filter(contributor_id %in% survived_2019) |>
    dplyr::filter(snap_year == 2018) |>
      dplyr::mutate(snap_year = 2019)
   
   leadership_gender_19 <- rbind(leadership_gender,fill_2019)
  
   ## last known status = max snap_year for each person
   
   toe_summary$snap_year <- toe_summary$max_year
   
   last_status <- dplyr::inner_join(leadership_gender,toe_summary,c("contributor_id","snap_year")) |>
     dplyr::select(contributor_id,leadership_role)
   
   
  lg_df <- leadership_gender_19 |>
    dplyr::select(publication_year = snap_year, contributor_id,leadership_role)
  
  auths_leadership <- dplyr::left_join(auths_ioi_4,lg_df,c("contributor_id","publication_year"))
  
  auths_missing_status <- auths_leadership |>
    dplyr::filter(is.na(leadership_role)) |>
    dplyr::select(-leadership_role)
  
  auths_last_status <- dplyr::left_join(auths_missing_status,last_status,"contributor_id") 
  
  auths_leadership_has_status <- auths_leadership |> 
    dplyr::filter(!is.na(leadership_role)) 
  
  auths_leadership_all_status <- rbind(auths_leadership_has_status,auths_last_status)
  
  ### add time at eha
  
  toe_contrib <- toe_summary |>
    dplyr::select(contributor_id,diff_year)
  
  
  auths_leadership_all_status_toe <- dplyr::left_join(auths_leadership_all_status,toe_contrib,by = "contributor_id")
  
  auths_leadership_all_status_toe |>
    dplyr::group_by(contributor_id,gender_final,leadership_role,diff_year) |>
    dplyr::summarise(pubs = n(),
              pub_rate = pubs/diff_year) |>
    dplyr::ungroup() |>
    dplyr::group_by(gender_final,leadership_role) |>
    dplyr::summarise(
      # mean_pubs = mean(pubs),
      # max_pubs = max(pubs),
      # min_pubs = min(pubs),
      # median_pubs = median(pubs),
      mean_pubs_year = mean(pub_rate),
      max_pubs_year = max(pub_rate),
      min_pubs_year = min(pub_rate),
      median_pubs_year = median(pub_rate)
    ) |>
    dplyr::arrange(leadership_role) 
  

}
