#' plot proportion female over time and country economic status
#'
#' @title plot_propfem_development_over_time
#' @param gender_development_time_summary
#' @param development_colors
#' @return
#' @author Cecilia Sanchez
#' @export
plot_propfem_development_over_time <- function(gender_development_time_summary,
                                               development_colors){

  yearly_tots <- gender_development_time_summary %>%
    group_by(year, development_status) %>%
    summarize(n_tot = sum(n))

  dat <- gender_development_time_summary %>%
    left_join(yearly_tots) %>%
    mutate(prop = n / n_tot) %>%
    filter(gender_final == "gendered female")

  ggplot(dat, aes(x = year, y = prop, group = development_status)) +
    geom_smooth(aes(color = development_status, fill = development_status), 
                method = "lm", formula = y~x) +
    geom_point(aes(fill = development_status), size = 3, pch = 21, color = "black") +
    scale_color_manual(name = "",
                       values = development_colors) +
    scale_fill_manual(name = "",
                      values = development_colors) +
    ylab("Proportion of authorships by gendered female authors") +
    ylim(c(0, 1)) +
    theme_bw() +
    theme(legend.position = "bottom")
}
