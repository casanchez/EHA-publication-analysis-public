#' plot gender by authorship position (first and last)
#'
#' @title plot_gender_by_position
#' @param gender_position_summary
#' @param gender_colors
#' @return
#' @author Cecilia Sanchez
#' @export
plot_gender_by_position <- function(gender_position_summary, gender_colors){
  
  ggplot(gender_position_summary, aes(x = gender_final, y = perc_of_all_FLA, 
                                      fill = gender_final, 
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
    ylab("Percent of all first and last authorships") + xlab("") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.text = element_text(color = "black", size = 12),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 14)
    )
}