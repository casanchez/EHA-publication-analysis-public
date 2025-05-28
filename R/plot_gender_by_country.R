#' plot gender by country
#'
#' @title plot_gender_by_country
#' @param gender_country_summary
#' @param gender_colors
#' @return
#' @author Cecilia Sanchez
#' @export
plot_gender_by_country <- function(gender_country_summary, gender_colors){
  
  `%not_in%` <- purrr::negate(`%in%`)

  dat <- gender_country_summary %>% 
    mutate(country_name = case_when(
      country_name %not_in% c("United States", "Bangladesh", "Australia", 
                              "China", "United Kingdom") ~ "Other",
      TRUE ~ country_name)
    ) %>% 
    mutate(country_name = factor(country_name, 
                                 levels = c("United States", "Bangladesh", 
                                            "Australia", "China",
                                            "United Kingdom", "Other")))
    
  # colors indicate gender
  ggplot(dat, aes(x = country_name, y = n)) +
    geom_col(position = position_stack(reverse = T), width = 0.6,
             aes(fill = gender_final)) +
    scale_x_discrete(labels = c("United\nStates", "Bangladesh", "Australia",
                                "China", "United\nKingdom",  "Other")) + 
    scale_fill_manual(name = "",
                      labels = c("Gendered female",  "Gendered male", 
                                 "Gendered nonbinary", "Unknown"),
                      values = gender_colors) +
    ylab("Number of first and last authorships") + 
    xlab("") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(color = "black", size = 12),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 14),
          #axis.text.x = element_text(angle = 45, hjust = 1)
          )
  
}

# version to only plot countries with a minimum number of authorships
# plot_gender_by_country <- function(gender_country_summary, tot_auth, gender_colors){
#   
#   country_tots <- gender_country_summary %>% 
#     group_by(country_name) %>% 
#     summarize(n_tot = sum(n))
#   
#   # filter to only countries with at least tot_auth authorships
#   gender_country_short <- gender_country_summary %>% 
#     left_join(country_tots) %>% 
#     filter(!is.na(development_status)) %>%  # for now
#     filter(n_tot >= tot_auth)
#   
#   # colors indicate gender
#   ggplot(gender_country_short, aes(x = country_name, y = n)) +
#     geom_col(position = position_stack(reverse = T), width = 0.6,
#              aes(fill = gender_final)) +
#     scale_fill_manual(name = "",
#                       values = gender_colors) +
#     ylab("Number of authorships") + xlab("Country affiliation") +
#     theme_bw() +
#     theme(legend.position = "bottom",
#           axis.text.x = element_text(angle = 45, hjust = 1))
#   
# }