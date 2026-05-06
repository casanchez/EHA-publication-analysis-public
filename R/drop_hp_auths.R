drop_hp_auths <- function(auths_with_gender, auth_count = 10){
  
  contrib_ids_9 <- auths_with_gender |>
    dplyr::group_by(contributor_id) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::filter(n < auth_count) |>
    dplyr::ungroup() |>
    dplyr::select(contributor_id)
  
  out <- dplyr::inner_join(x = auths_with_gender, y = contrib_ids_9,by = "contributor_id")
  
  return(out)

  }