#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mod
#' @return
#' @author collinschwantes
#' @export
plot_model_diagnostics <- function(mod, 
                                   dp_file = "figures/FigS1.tiff") {

  # Define the output file
  tiff(dp_file, width = 6, height = 6, units = "in", res = 600, 
       compression = "lzw")
  
  # Set layout and plot
  par(mfrow = c(2, 2))
  plot(mod)
  
  # 4. Close the device
  dev.off()

  return(dp_file)
}
