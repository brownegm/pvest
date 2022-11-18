#Slope and intercept functions
#' SMA intercept and slope
#' @description Standard major axis(SMA) parameter estimation. The functions provided here
#'    will estimate the slope and intercept of the SMA line for a given dataset.
#' @param x x variable
#' @param y y variable
#' @param slope slope parameter estimated by the 'sma_slope' function
#'
#' @details The slope of the relationship is determined as the the standard deviation of
#'     of the x variable divided by the standard deviation of the y variable. Estimation of the intercept
#'     requires the estimation of the SMA slope. The intercept is estimated as the mean of the x variable
#'     minus the SMA slope times the mean value of the y variable. Often in pressure volume curves, the x variable is the water-associated
#'     variable(e.g., water content or leaf relative water content). Likewise, the y variable is often associated
#'     with pressure(e.g., leaf water potential, and inverse of leaf water potential)
#'
#' @return Returns slope and intercept values for standard major axis regression
#'
#' @examples 
#' x<-c(0.187,0.170,0.165,0.095)
#' y<-c(-0.020, -0.150,-0.800,-1.520)
#' slope<-sma_slope(x, y)
#' intercept<-sma_intercept(x, y, slope)
#' print(slope, intercept)

sma_slope <- function(x, y) {
  return(sd(x) / sd(y))
}

sma_intercept <- function(x, y, slope) {
  return(mean(x) - (slope * mean(y)))
}
