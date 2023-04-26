
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


