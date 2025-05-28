#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param leadership_gender
#' @return
#' @author Collin Schwantes
#' @export
summarize_leadership_gender <- function(leadership_gender) {

  individual_df  <- leadership_gender |>
    dplyr::group_by(contributor_id,gender_final,leadership_role) |>
    dplyr::summarise(min_year = min(snap_year),
              max_year = max(snap_year),
              diff_year = (max_year - min_year)+1) |>
    dplyr::ungroup()
  
  individual_df |>
    dplyr::group_by(gender_final,leadership_role) |>
    dplyr::summarise(n =  n(),
                     median_time_in_leadership = median(diff_year),
                     mean_time_in_leadership = mean(diff_year),
                     hdci_time_in_leadership = ggdist::hdci(diff_year),
                     max_time_in_leadership = max(diff_year),
                     min_time_in_leadership = min(diff_year))

}
