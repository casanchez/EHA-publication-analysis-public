#' plot model predictions
#'
#' @title plot_model_preds
#'
#' @param income_colors
#' @param mod 
#' @param gender_colors
#'
#' @return
#' @author Cecilia Sanchez
#' @export
plot_model_preds <- function(mod, income_colors, gender_colors, mod_terms = c("income_majority","authorship_position") ){
  
  
  ggpredict_df <- ggeffects::ggpredict(model = mod, terms = mod_terms)

    
  ggplot2::ggplot(data = ggpredict_df) +
    geom_point(aes(x = x,
                   y = predicted,
                   alpha = group),
               size = 3,
               color = gender_colors[1],
               position = position_dodge(width = .25)) +
    # scale_color_manual(values = gender_colors[c(1,1)]) +
    scale_alpha_manual(values = c(0.4, 1)) + 
    geom_linerange(aes(x = x,
                    ymin = conf.low,
                    ymax = conf.high,
                    alpha = group),
                   color = gender_colors[1],
                   linewidth = 1,
                   position = position_dodge(width = .25),
                   inherit.aes = FALSE,
                   data = ggpredict_df ) +
    theme_bw() +
    ylim(c(0, 75)) +
    labs(alpha = "Authorship position") +
    xlab("Country Income") +
    ylab("Predicted percent of authorships \nby gendered female authors") +
    scale_x_discrete(position = "bottom", 
                     expand = c(.37,.37),
                     labels = c('Low and Middle Income', 'High Income')) +
       theme(legend.position = "inside",
          legend.position.inside = c(0.5, 0.8),
          # legend.title = element_blank(),
          legend.text = element_text(color = "black", size = 8),
          axis.text = element_text(color = "black", size = 8),
          axis.title = element_text(color = "black", size = 8)) 
  
  
}

# sjPlot::plot_model(mod, type = "pred",
#                          terms = c( "income_majority","authorship_position"),
#                          colors = c("#66317166","#663171"),
#                          title = "",
#                          axis.title = c("Country Income", 
#                                         "Predicted percent of authorships \nby gendered female authors"),
#                          legend.title = "Authorship position",
#                          line.size = 1, dot.size = 3
#                           )

# p2 <- sjPlot::plot_model(model, type = "pred",
#                          terms = "income_majority",
#                          #colors = income_colors[c(3,1)],
#                          colors = "black",
#                          title = "",
#                          axis.title = c("",
#                                         "Predicted % of authorships by gendered female authors"),
#                          line.size = 1.5) +
#   theme_bw() +
#   ylim(c(0, 100)) +
#   theme(axis.text = element_text(color = "black", size = 12),
#         axis.title = element_text(color = "black", size = 14))
#   
# p3 <- sjPlot::plot_model(model, type = "pred",
#                          terms = c("year_centered", "authorship_position"),
#                          # colors = gender_colors,
#                          colors = c("mediumpurple1", "purple4"),
#                          title = "",
#                          axis.title = c("Year",
#                                         "Predicted % of authorships by gendered female authors"),
#                          line.size = 1.5) +
#   theme_bw() +
#   ylim(c(0, 100)) +
#   scale_x_continuous(breaks = c(0:11), labels = 2011:2022) +
#   theme(legend.position = "inside",
#         legend.position.inside = c(0.7, 0.85),
#         axis.text = element_text(color = "black", size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title = element_text(color = "black", size = 14))
# 
# cowplot::plot_grid(p1, p2, p3, labels = c('A', 'B', 'C'), label_size = 14,
#                    nrow = 1, rel_widths = c(1.5, 1, 2))