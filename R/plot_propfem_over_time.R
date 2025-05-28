#' plot proportion female over time
#'
#' @title plot_propfem_over_time
#' @param gender_development_time_summary
#' @param gender_colors
#' @return
#' @author Cecilia Sanchez
#' @export
plot_propfem_over_time <- function(gender_time_summary, gender_colors){
  
  # for now
  yearly_tots <- gender_time_summary %>%
    group_by(year) %>%
    summarize(n_tot = sum(n))
  
  dat <- gender_time_summary %>%
    left_join(yearly_tots) %>%
    mutate(prop = n / n_tot) %>%
    filter(gender_final == "gendered female")
  
  ggplot(dat, aes(x = year, y = prop, group = gender_final)) +
    geom_smooth(method = "lm", formula = y~x, fill = "gray80", color = "gray20") +
    # geom_line(color = "#8700F9", lwd = 1.5) +
    # geom_point(color = "black") +
    geom_point(color = gender_colors[1]) +
    ylab("Proportion of authorships by gendered female authors") +
    ylim(c(0, 1)) +
    theme_bw() +
    theme(legend.position = "bottom")
}
