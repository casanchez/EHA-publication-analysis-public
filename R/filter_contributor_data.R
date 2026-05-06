#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param contributor_data_intermed
#' @param contrib_keep
#' @return
#' @author collinschwantes
#' @export
filter_contributor_data <- function(contributor_data_intermed, contrib_keep) {

  contributor_data = contributor_data_intermed %>%
    filter(contributor_id %in% contrib_keep)
  
  return(contributor_data)
}
