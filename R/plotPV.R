#' plotPV
#'
#' @param data dataframe
#' @param x x value
#' @param y y value
#' @param rows number of rows to select
#' @param ... parameters to pass through plot() 
#'
#' @return a plot
#' 
#' @export
#'
#'

plotPV <- function(data, x, y, rows = 4, ...) {
  
  plot.x<- utils::tail(data[[x]], n=rows)
  plot.y<-utils::tail(data[[y]], n=rows)
  
  slope <- pvest::sma_slope(plot.y, plot.x)*-1
  int <- pvest::sma_intercept(plot.y, plot.x, slope)

  l.x <- seq(0, max(data[[x]]), length.out = 50)
  l.y <- int + (slope * l.x)

  plot(data[[y]] ~ data[[x]],
    ylab = expression(paste(1/Psi, " (MPa)")),
    xlab= "Relative Water Deficit (%) ",
    ...
  )

  graphics::lines(x = l.x, y = l.y)
  
  graphics::legend("topright", legend=bquote(pi[o] == .(format(-1/int, digits = 3))), bty="n")
  
}


