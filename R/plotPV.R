
#' plotPV
#'
#' @param data 
#' @param x 
#' @param y 
#' @param rows number of rows to select
#' @param ... 
#'
#' @return a plot
#' @export
#'
#' @examples
#' plotPV(data[1:4,"leaf"=5], x='fresh.weight', y='water.potential')
#' 
#'   
#' 

plotPV<-function(data, x, y, rows=4, ...){
  
  
  slope<-pvest::sma_slope(data[[x]][1:rows], data[[y]][1:rows])
  int<-pvest::sma_intercept(data[[x]][1:rows], data[[y]][1:rows], slope)
  
  l.x = seq(0, max(data[[x]]),length.out=length(data[[x]]))
  l.y = int+(slope*l.x)

  plot(data[[y]]~data[[x]], 
       ylab=y, 
       xlab=x)
  
  lines(x=l.x, y=l.y)
  
}

#other thoughts using ggplot
library(ggplot2)

plot_points_lines_lm <- function(x, y, x2 = NULL, color1 = "blue", color2 = "red") {
  # create data frame with the x and y coordinates
  data <- data.frame(x = x, y = y)
  
  # create linear model and extract slope and intercept
  lm_model <- lm(y ~ x, data = data)
  slope <- lm_model$coefficients[2]
  intercept <- lm_model$coefficients[1]
  
  # create basic plot with points
  plot <- ggplot(data, aes(x, y)) +
    geom_point(color = color1)
  
  # add line representing linear model
  line_data <- data.frame(x = range(x))
  line_data$y <- intercept + slope * line_data$x
  plot <- plot + geom_line(data = line_data, aes(x, y), color = color2)
  
  return(plot)
}

