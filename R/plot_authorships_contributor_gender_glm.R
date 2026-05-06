#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#'
#' @param glm_dat
#' @param glm_mod 
#' @param gender_colors
#'
#' @return
#' @author collinschwantes
#' @export
plot_authorships_contributor_gender_glm <- function(glm_dat, glm_mod, gender_colors) {

  # list predicted values for training data ~ give me back the means
  list_glm_predict <- predict(glm_mod, type = "response",se.fit = TRUE) 
  
  df_mean_se <- data.frame(
    mean_auths = list_glm_predict$fit |> unique(),
    se_auths = list_glm_predict$se.fit |> unique(),
    gender_final = c("gendered female","gendered male")
  )
  
  
  ggplot(glm_dat, 
         aes(x = n, fill = gender_final)) +
    geom_histogram(position = "dodge", binwidth = 1, color = "black") +
    scale_y_sqrt(breaks = c(0, 1, 10, 50, 100, 150, 200)) +
    scale_x_continuous(breaks = scales::breaks_width(5)) +
    scale_fill_manual(name = "", values = gender_colors[1:2]) +
    geom_vline(aes(xintercept = mean_auths), data = df_mean_se,linetype = 2, color = "orange") +
    geom_vline(aes(xintercept = mean_auths-se_auths), data = df_mean_se,linetype = 2, color = "grey") +
    geom_vline(aes(xintercept = mean_auths+se_auths), data = df_mean_se,linetype = 2, color = "grey") +
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
