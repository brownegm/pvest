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


sma_intercept <- function(x, y, slope) {

  return(mean(x) - (slope * mean(y)))
}


#' Standard major axis model estimation. 
#' @description Standard major axis(SMA) parameter estimation. The function here will estimate the slope and intercept of the SMA line for a given dependent and independent variable. There is a generic function that will dispatch the appropriate function based on the input data type. There is also an alternative method for objects of class "\strong{osm_input}" which is a list of vectors containing the negative inverse of the leaf water potential and the relative water deficit.
#' 
#' @param x independent variable
#' @param y dependent variable
#'
#' @returns Returns a model function slope and intercept values for the standard major axis regression
#' 
#' @export
#' @rdname sma_model

sma_model <- function(x, y,...){
  UseMethod("sma_model")
}


#' @rdname sma_model
#' @export
#
sma_model.default <- function(x, y, ...){
  
  if(any(is.na(x))|any(is.na(y))){
    stop("sma_model:Missing values found in input data. Ensure that there are no missing values.")
  }
  
  slope <- pvest::sma_slope(x, y)
  intercept <- pvest::sma_intercept(x, y, slope)
  
  #terms.formula(y~slope * x + intercept)
  
  model <- structure(.Data = list(slope, intercept),
                     .Names = c("slope", "intercept"),
                     class = "sma_model")
  
  invisible(model)

}


#' @rdname sma_model
#' @export
#' 
sma_model.osm_input <- function(x,y= NULL, ...){
   
  if(any(is.na(x[[1]]))|any(is.na(x[[2]]))){
    stop("sma_model:Missing values found in input data. Ensure that there are no missing values.")
  }

  slope <- pvest::sma_slope(x$neg_inv_psi, x$rwd) * - 1
  intercept <- pvest::sma_intercept(x$neg_inv_psi, x$rwd, slope)
  
  model <- structure(.Data = list(slope, intercept),
                     .Names = c("slope", "intercept"),
                     class = "sma_model")
  
  invisible(model)
}

#' Print method for the SMA model output
#'
#' @param x An object of the class "sma_model"
#' @param ... Other parameters passed to cat
#'
#' @returns Printed SMA model output
#' @export print.sma_model

print.sma_model <- function(x, ...){
  cat("SMA model\n")
  cat("Slope:", x$slope, "\n")
  cat("Intercept:", x$intercept, "\n")
}


