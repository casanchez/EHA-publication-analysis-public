#' plot % of FLA separated by country income group
#'
#' @title plot_income_by_position
#' @param income_position_summary
#' @param income_colors
#' @return
#' @author Cecilia Sanchez
#' @export
plot_income_by_position <- function(income_position_summary, income_colors){
  
  income_data <- income_position_summary %>% 
    mutate(income_majority = forcats::fct_relevel(
      income_majority, "H", "UM", "LM", "L"),
      income_majority = forcats::fct_recode(
        income_majority, "High-income" = "H", "Upper-middle-income" = "UM", 
        "Lower-middle-income" = "LM", "Low-income" = "L"))
    
  ggplot(income_data, 
         aes(x = income_majority, y = perc_of_all_FLA, 
             fill = income_majority,
             alpha = authorship_position)) +
    geom_col(position = position_dodge()) +
    scale_alpha_manual(name = "Authorship position",
                       labels = c("First", "Last"),
                       values = c(0.4, 1)) +
    scale_fill_manual(name = "",
                      values = income_colors,
                      guide = "none") +
    ylab("Percent of all first and last authorships") + xlab("") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(color = "black", size = 12),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 14)
    )
}