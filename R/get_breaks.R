#' Get breaks
#' 
#' @param vals Numeric. Vector of numbers
#' @param break_high Numeric. High break point
#' @param break_low Numeric. Low break point
#'
#' @return Numeric vector
#' @author Collin Schwantes
#' @export
get_breaks <- function(vals, break_high = 5, break_low = 1){
  
  break_points <- c(0,0)
  high_point <- -1
  low_point <- -1
  for( i in 2:length(vals)){
    x <- vals[i]
    
    
    print(x)
    if(x > break_high){
      if(high_point > -1){
        next()
      }
      print("adding 5 point")
      high_point <- i-1
      break_points[2] <- high_point
      next()
    }
    
    if(x > break_low) {
      if(low_point > -1){
        next()
      }
      print("adding 1 point")
      low_point <- i-1
      break_points[1] <- low_point
      next()
    }
  }
  
  return(break_points)
}

