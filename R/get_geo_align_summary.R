#' calculate alignment of authorship geography and article geography
#'
#' @title get_geo_align_summary
#' @param geo_focused_pubs
#' @param time_period
#' @return
#' @author Collin Schwantes
#' @export

get_geo_align_summary <- function(geo_focused_pubs, time_period){
  
  first_geo_match_tbl <- table(geo_focused_pubs$first_geo_match)
  last_geo_match_tbl <- table(geo_focused_pubs$last_geo_match)
  either_geo_match_tbl <- table(geo_focused_pubs$either_geo_match)
  both_geo_match_tbl <- table(geo_focused_pubs$both_geo_match)
  
  n_first_geo_match <- first_geo_match_tbl[2]
  n_last_geo_match <- last_geo_match_tbl[2]
  n_either_geo_match <- either_geo_match_tbl[2]
  n_both_geo_match <- both_geo_match_tbl[2]
  
  denom_first_geo_match <- sum(first_geo_match_tbl)
  denom_last_geo_match <- sum(last_geo_match_tbl)
  denom_either_geo_match <- sum(either_geo_match_tbl)
  denom_both_geo_match <-  sum(last_geo_match_tbl)
  
  first_match_perc <- round(n_first_geo_match / sum(first_geo_match_tbl)*100, 1)
  last_match_perc <- round(n_last_geo_match / sum(last_geo_match_tbl)*100, 1)
  either_match_perc <- round(n_either_geo_match / sum(either_geo_match_tbl)*100, 1)
  both_match_perc <- round(n_both_geo_match / sum(both_geo_match_tbl)*100, 1)
  
  out <- data.frame(time_period = time_period, 
                    first_authorship_match = sprintf("%s/%s (%s%%)", 
                                                     n_first_geo_match,
                                                     denom_first_geo_match, 
                                                     first_match_perc),
                    last_authorship_match = sprintf("%s/%s (%s%%)", 
                                                    n_last_geo_match,
                                                    denom_last_geo_match, 
                                                    last_match_perc),
                    either_match = sprintf("%s/%s (%s%%)", n_either_geo_match,
                                           denom_either_geo_match, 
                                           either_match_perc),
                    both_match = sprintf("%s/%s (%s%%)", n_both_geo_match,
                                         denom_both_geo_match, both_match_perc)
  )
  
  return(out)
}