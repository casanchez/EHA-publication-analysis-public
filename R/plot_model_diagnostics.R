#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mod
#' @return
#' @author collinschwantes
#' @export
plot_model_diagnostics <- function(mod, dp_file = "diagnostic_plots.png") {

  # Define the output file
  png(dp_file, width = 800, height = 800)
  
  # Set layout and plot
  par(mfrow = c(2, 2))
  plot(mod)
  
  # 4. Close the device
  dev.off()

  return(file_time)
}
