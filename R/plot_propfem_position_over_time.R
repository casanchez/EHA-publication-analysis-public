#' plot proportion female over time and authorship position
#'
#' @title plot_propfem_position_over_time
#' @param gender_position_time_summary
#' @param position_colors 
#' @return
#' @author Cecilia Sanchez
#' @export
plot_propfem_position_over_time <- function(gender_position_time_summary, 
                                            position_colors){

  yearly_tots <- gender_position_time_summary %>%
    group_by(year, authorship_position) %>%
    summarize(n_tot = sum(n))
  
  dat <- gender_position_time_summary %>%
    left_join(yearly_tots) %>%
    mutate(perc = n/n_tot*100) %>%
    filter(gender_final == "gendered female")
  
  ggplot(dat, aes(x = year, y = perc, group = authorship_position)) +
    geom_point(aes(fill = authorship_position), size = 3, pch = 21, 
               color = "black") +
    geom_line(aes(color = authorship_position)) +
    scale_color_manual(name = "", values = position_colors, 
                       labels = c("First", "Last")) +
    scale_fill_manual(name = "", values = position_colors, 
                      labels = c("First", "Last")) +
    ylab("Percent of authorships \nby gendered female authors") + xlab("") +
    ylim(c(0, 100)) +
    theme_bw() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.25, 0.8),
          legend.title = element_blank(),
          legend.text = element_text(color = "black", size = 8),
          axis.text = element_text(color = "black", size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(color = "black", size = 8))
}