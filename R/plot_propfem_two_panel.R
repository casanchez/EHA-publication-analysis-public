#' Two panel plot to show proportion female over time by position and GNGS
#'
#' @title plot_propfem_two_panel
#' @param gender_position_time_summary
#' @param gender_income_time_summary
#' @param position_colors
#' @param income_colors 
#' @return
#' @author Cecilia Sanchez
#' @export

plot_propfem_two_panel <- function(gender_position_time_summary,
                                   gender_income_time_summary,
                                   position_colors,
                                   income_colors){
  
  yearly_tots <- gender_position_time_summary %>%
    group_by(year, authorship_position) %>%
    summarize(n_tot = sum(n))
  
  dat <- gender_position_time_summary %>%
    left_join(yearly_tots) %>%
    mutate(perc = n/n_tot*100) %>%
    filter(gender_final == "gendered female")
  
  p1 <- ggplot(dat, aes(x = year, y = perc, group = authorship_position)) +
    geom_point(aes(fill = authorship_position), size = 3, pch = 21, 
               color = "black") +
    geom_line(aes(color = authorship_position)) +
    scale_color_manual(name = "", values = position_colors, 
                       labels = c("First", "Last")) +
    scale_fill_manual(name = "", values = position_colors, 
                      labels = c("First", "Last")) +
    ylab("Perc. of authorships by gendered female authors") + xlab("") +
    ylim(c(0, 100)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(color = "black", size = 12),
          axis.text = element_text(color = "black", size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(color = "black", size = 14))
    
  

  
  yearly_tots <- gender_income_time_summary %>%
    group_by(year, income_majority) %>%
    summarize(n_tot = sum(n))
  
  dat <- gender_income_time_summary %>%
    left_join(yearly_tots) %>%
    mutate(perc = n/n_tot*100) %>%
    filter(gender_final == "gendered female") %>% 
    mutate(income_majority = forcats::fct_relevel(
      income_majority, "H", "UM", "LM", "L"))
  
  p2 <- ggplot(dat, aes(x = year, y = perc, group = income_majority)) +
    geom_point(aes(fill = income_majority), size = 3, pch = 21, 
               color = "black") +
    geom_line(aes(color = income_majority)) +
    facet_wrap(~income_majority) +
    scale_color_manual(name = "", values = income_colors) +
    scale_fill_manual(name = "", values = income_colors) +
    ylab("Perc. of authorships by gendered female authors") + xlab("") +
    ylim(c(0, 100)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(color = "black", size = 12),
          axis.text = element_text(color = "black", size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(color = "black", size = 14))
  
  cowplot::plot_grid(p1, p2, labels = c('A', 'B'), label_size = 14)
}