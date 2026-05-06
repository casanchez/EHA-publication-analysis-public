#' Compares publication rates by gender, using generalized linear models and 
#' assuming a poisson distribution. 
#' 
#' prints summary and anova.glm outputs for the model.
#' 
#' 
#' 
#' @param authorships_contributor_gender_summary
#' @return model object
#' @author collinschwantes
#' @export
compare_distributions <- function(authorships_contributor_gender_summary) {

  # model publication rate ~ gender assuming a poisson distribution
  m1  <- glm(n~gender_final,family = "poisson",data =authorships_contributor_gender_summary )
    
  # look at coefficients
  print(summary(m1))
  
  # check if pub_rate ~ gender performs better than a null model
  print(anova(m1,test = "Chisq"))
  
  return(m1)
}
