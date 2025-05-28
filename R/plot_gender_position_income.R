#' plot authorships by gender, authorship position, and income group
#'
#' @title plot_gender_position_income
#' @param gender_position_income_summary
#' @param gender_colors palette to use for gender
#' @return
#' @author Cecilia Sanchez
#' @export
plot_gender_position_income <- function(gender_position_income_summary, 
                                        gender_colors){
  
  income_data <- gender_position_income_summary %>% 
    mutate(income_majority = forcats::fct_relevel(
      income_majority, "H", "UM", "LM", "L"),
      income_majority = forcats::fct_recode(
        income_majority, "High-income" = "H", "Upper-middle-income" = "UM", 
        "Lower-middle-income" = "LM", "Low-income" = "L")
      )
  
  ggplot(income_data, 
               aes(x = gender_final, y = perc, fill = gender_final,
                   alpha = authorship_position)) +
    geom_col(position = position_dodge()) +
    scale_x_discrete(labels = c("Gendered\nfemale",  "Gendered\nmale", 
                                "Gendered\nnonbinary", "Unknown")) + 
    scale_fill_manual(name = "Author gender",
                      labels = c("Gendered female",  "Gendered male", 
                                 "Gendered nonbinary", "Unknown"),
                      values = gender_colors,
                      guide = "none") +
    scale_alpha_manual(name = "Authorship position",
                       labels = c("First", "Last"),
                       values = c(0.4, 1)) +
    facet_wrap(~income_majority, nrow = 2) +
    ylab("Percent of all first and last authorships") +
    xlab("") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.title = element_text(color = "black", size = 8),
          legend.text = element_text(color = "black", size = 8),
          axis.text = element_text(color = "black", size = 8),
          axis.title = element_text(color = "black", size = 12),
          strip.text.x = element_text(size = 10, color = "black", face = "bold"),
          strip.background = element_rect(fill = c("white"))
    )
}
