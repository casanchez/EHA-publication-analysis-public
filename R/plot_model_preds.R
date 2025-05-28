#' plot model predictions
#'
#' @title plot_model_preds
#' @param model
#' @param income_colors
#' @param gender_colors
#' @return
#' @author Cecilia Sanchez
#' @export
plot_model_preds <- function(model, income_colors, gender_colors){
  
  sjPlot::plot_model(model, type = "pred",
                           terms = c("authorship_position", "income_majority"),
                           colors = income_colors[c(3,1)], 
                           title = "",
                           axis.title = c("Authorship position", 
                                          "Predicted percent of authorships \nby gendered female authors"),
                           legend.title = "",
                           line.size = 1, dot.size = 3) +
    theme_bw() +
    # geom_text(aes(label = round(predicted, 2)),
    #           position = position_dodge(width = 1), size = 8) +
    ylim(c(0, 100)) +
    theme(legend.position = "inside",
          legend.position.inside = c(0.5, 0.8),
          legend.title = element_blank(),
          legend.text = element_text(color = "black", size = 8),
          axis.text = element_text(color = "black", size = 8),
          axis.title = element_text(color = "black", size = 8))
  
}

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