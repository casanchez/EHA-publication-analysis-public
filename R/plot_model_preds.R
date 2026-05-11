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
plot_model_preds <- function(mod, income_colors, gender_colors, 
                             mod_terms = c("income_majority",
                                           "authorship_position")){
  
  
  ggpredict_df <- ggeffects::ggpredict(model = mod, terms = mod_terms)

    
  ggplot2::ggplot(data = ggpredict_df) +
    geom_point(aes(x = x,
                   y = predicted,
                   alpha = group),
               size = 3,
               color = gender_colors[1],
               position = position_dodge(width = .25)) +
    # scale_color_manual(values = gender_colors[c(1,1)]) +
    scale_alpha_manual(name = "",
                       labels = c("First authorship", "Last authorship"),
                       values = c(0.4, 1)) +
    geom_linerange(aes(x = x,
                       ymin = conf.low,
                       ymax = conf.high,
                       alpha = group),
                   color = gender_colors[1],
                   linewidth = 1,
                   position = position_dodge(width = .25),
                   inherit.aes = FALSE,
                   data = ggpredict_df) +
    theme_bw() +
    ylim(c(0, 100)) +
    xlab("Country income") +
    ylab("Predicted percent of authorships \nby gendered female authors") +
    scale_x_discrete(position = "bottom", 
                     expand = c(.37, .37),
                     labels = c('Low- and \nmiddle-income', 'High-income')) +
    theme(legend.position = "inside",
          legend.position.inside = c(0.35, 0.8),
          legend.title = element_blank(),
          legend.text = element_text(color = "black", size = 8),
          axis.text = element_text(color = "black", size = 8),
          axis.title = element_text(color = "black", size = 9)) 
  
  
}