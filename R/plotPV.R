




#' plotPV
#'
#' @param data 
#' @param x 
#' @param y 
#' @param ... 
#'
#' @return a plot
#' @export
#'
#' @examples
#' 
#' plotPV(data%>%filter(leaf=="5"), x='fresh.weight', y='water.potential')
#' 

plotPV<-function(data, x, y,...){
  
  slope<-pvest::sma_slope(data[[x]], data[[y]])
  int<-pvest::sma_intercept(data[[x]], data[[y]], slope)
  
  l.x = seq(0, max(data[[x]]),length.out=length(data[[x]]))
  l.y = int-(slope*l.x)

  plot(data[[y]]~data[[x]], 
       ylab=y, 
       xlab=x)
  
  lines(x=l.x, y=l.y)
  
}


