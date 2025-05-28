#' plot number of authorships by contributor and gender
#'
#' @title plot_authorships_contributor_gender
#' @param authorships_contributor_gender_summary
#' @param gender_colors
#' @return
#' @author Cecilia Sanchez
#' @export
plot_authorships_contributor_gender <- function(authorships_contributor_gender_summary,
                                                gender_colors){
  
  ggplot(authorships_contributor_gender_summary, 
         aes(x = n, fill = gender_final)) +
    geom_histogram(position = "dodge", binwidth = 1, color = "black") +
    scale_y_sqrt(breaks = c(0, 1, 10, 50, 100, 150, 200)) +
    scale_fill_manual(name = "", values = gender_colors[1:2]) +
    facet_wrap(~gender_final, nrow = 2) +
    ylab("Number of unique authors") +
    xlab("Total first and last authorships") +
    theme_bw() +
    theme(legend.position = "none",
          legend.text = element_text(color = "black", size = 12),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 14),
          strip.text.x = element_text(size = 12, color = "black", face = "bold"),
          strip.background = element_rect(fill = "white")
          )
  
}
