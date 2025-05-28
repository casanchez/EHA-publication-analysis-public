#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param country_summary
#' @param variable
#' @param income_colors
#' @return
#' @author Collin Schwantes
#' @export
plot_by_country <- function(country_summary, variable, income_colors) {
  
  country_summary <- country_summary %>% 
    mutate(income_majority = forcats::fct_relevel(
      income_majority, "H", "UM", "LM", "L"),
      income_majority = forcats::fct_recode(
        income_majority, "High-income" = "H", "Upper-middle-income" = "UM", 
        "Lower-middle-income" = "LM", "Low-income" = "L"))

  # authorship 
  country_summary %>% 
    arrange(n) %>% 
    filter(!is.na(!!rlang::sym(variable))) %>% 
    # This trick updates the factor levels
    mutate(y = factor(!!rlang::sym(variable), 
                      levels = !!rlang::sym(variable))) %>%
  ggplot(aes(y = y, x = n)) +
    geom_point(aes(fill = income_majority), size = 4, pch = 21, color ="black") + 
    scale_fill_manual(name = "", values = income_colors) +
    scale_x_log10() +    
    ylab("") + xlab("Number of first and last authorships") +
    theme_bw() +
    guides(fill = guide_legend(position = "inside")) +
    theme(legend.position.inside = c(0.75, 0.1),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))

}
