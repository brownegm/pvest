# Slope and intercept functions
#' SMA slope
#' @description Standard major axis(SMA) parameter estimation. The functions provided here
#'    will estimate the slope and intercept of the SMA line for a given dataset. For a
#'    detailed explanation of the function see \code{vignette('sma-slope-intercept')}
#' @param x x variable
#' @param y y variable
#'
#' @details The slope of the relationship is determined as the the standard deviation of
#'     of the x variable divided by the standard deviation of the y variable. Estimation of the intercept
#'     requires the estimation of the SMA slope. The intercept is estimated as the mean of the x variable
#'     minus the SMA slope times the mean value of the y variable. Often in pressure volume curves, the x variable is the water-associated
#'     variable(e.g., water content or leaf relative water content). Likewise, the y variable is often associated
#'     with pressure(e.g., leaf water potential, and inverse of leaf water potential)
#'
#' @return Returns slope value for standard major axis regression
#'
#' @export
#'
#' @importFrom stats sd


sma_slope <- function(x, y) {

  return(sd(x) / sd(y))
}

NULL

#' SMA intercept estimation
#'
#' @param x x variable
#' @param y y variable
#' @param slope slope parameter estimated by the sma_slope function
#'
#' @return Returns slope and intercept values for standard major axis regression
#' @details  Estimation of the intercept \strong{requires} the estimation of the SMA slope. The intercept is estimated as the mean of the x variable
#'     minus the SMA slope times the mean value of the y variable. Often in pressure volume curves, the x variable is the water-associated
#'     variable(e.g., water content or leaf relative water content). Likewise, the y variable is often associated
#'     with pressure(e.g., leaf water potential, and inverse of leaf water potential). For a
#'     detailed explanation of the function see \code{vignette('sma-slope-intercept')}
#' @export
#'

sma_intercept <- function(x, y, slope) {

  return(mean(x) - (slope * mean(y)))
}


#' SMA model formulation
#'
#' @param x independent variable
#' @param y dependent variable
#'
#' @returns Returns a model function slope and intercept values for the standard major axis regression
#' @export
#'

sma_model <- function(x,y){
  
  if(any(is.na(x))|any(is.na(y))){
    stop("sma_model:Missing values found in input data. Ensure that there are no missing values.")
  }
  
  slope <- sma_slope(x,y)
  intercept <- sma_intercept(x,y,slope)
  model <- as.formula(paste("y ~", slope, "* x +", intercept))
  
  return(structure(model, .Names = "model", slope= slope, intercept = intercept, class = "sma_model"))
}
