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
#' @export


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
#'
#' @return Returns slope and intercept values for standard major axis regression
#' @details  Estimation of the intercept \strong{requires} the estimation of the SMA slope. The intercept is estimated as the mean of the x variable
#'     minus the SMA slope times the mean value of the y variable. Often in pressure volume curves, the x variable is the water-associated
#'     variable(e.g., water content or leaf relative water content). Likewise, the y variable is often associated
#'     with pressure(e.g., leaf water potential, and inverse of leaf water potential). For a
#'     detailed explanation of the function see \code{vignette('sma-slope-intercept')}
#' @export

sma_intercept <- function(x, y, slope) {

  return(mean(x) - (slope * mean(y)))
}


#' Standard major axis model estimation. 

#' @description This is a generic function that will dispatch 1 of 3 methods based on the input data type (i.e., _default_, _`osm_input`_, and _`tlp_input`_) to construct a standard major axis model from calculated slope and intercept for a given dependent and independent variable: 
#' * _default_ : slope and intercept are positive
#' * _`osm_input`_: slope is forced negative representing the relationship between relative water deficit and the negative inverse of water potential. Intercept is positive. 
#' * _`tlp_input`_: same as for `osm_input` but the input should provide symplastic relative water content as well and as a result the output includes the slope and intercept for both the bulk and symplastic variables. 
#' 
#' @param x independent variable
#' @param y dependent variable
#' @param ... additional parameters passed to methods
#'
#' @return Returns a model function slope and intercept values for the standard major axis regression
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
sma_model.osm_input <- function(x,y = NULL, ...){
   
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


#' @rdname sma_model
#' @export
sma_model.tlp_input <- function(x, y=NULL, ...){

  if(any(is.na(x[[1]]))|any(is.na(x[[2]]))){
    stop("sma_model:Missing values found in input data. Ensure that there are no missing values.")
  }
  
  slope <- pvest::sma_slope(x$psip, x$rwd) * - 1
  intercept <- pvest::sma_intercept(x$psip, x$rwd, slope)
  
  slope_sym <- pvest::sma_slope(x$psip, x$symrwd) * - 1
  intercept_sym <- pvest::sma_intercept(x$psip, x$symrwd, slope_sym)
  
  #calculate the slopes for capacitance full turgor
  slope_cap_ft <- pvest::sma_slope(x$rwc_above,x$psi_above)*0.01
  slope_cap_sym_ft <- pvest::sma_slope(x$symrwc_above,x$psi_above)*0.01
  #calculate the slopes for capacitance tlp
  slope_cap_tlp <- pvest::sma_slope(x$rwc_below,x$psi_below)*0.01
  slope_cap_sym_tlp <- pvest::sma_slope(x$symrwc_below,x$psi_below)*0.01

  model <- structure(.Data = list(slope, intercept,
                                  slope_sym, intercept_sym,
                                  slope_cap_ft, slope_cap_sym_ft,
                                  slope_cap_tlp, slope_cap_sym_tlp),
                     .Names = c("slope", "intercept", 
                                "slope_sym", "intercept_sym", 
                                "slope_cap_ft", "slope_cap_sym_ft",
                                "slope_cap_tlp", "slope_cap_sym_tlp"),
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
#' @rdname sma_model
#' @importFrom withr local_options

print.sma_model <- function(x, ...){
  
  withr::local_options(list(digits = 4))
  
  cat("Standard Major Axis (SMA) model\n")
  cat("-------------------------------")
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  cat("Slope:     ", x$slope, "\n")
  cat("Intercept: ", x$intercept, "\n")
  cat("\nModel Fit Metrics:\n")
  cat("Residual Standard Error (RMSE):", attr(x,"rmse"), "\n")
  cat("R-squared:                     ", attr(x,"r_squared"), "\n")
  cat("AICc:                          ",attr(x,"aicc"), "\n")
}


# Step 1: Create a custom function to calculate SMA slope and intercept
sm <- function(x, y = NULL) {
  
    # Check input data
    if (!is.numeric(x) || !is.numeric(y)) {
      stop("Both x and y must be numeric vectors.")
    }
    if (length(x) != length(y)) {
      stop("x and y must have the same length.")
    }

  # establish model object
  m <- vector(mode = "list", length = 7)
  names(m) <- c("slope", "intercept",
                "data", "call","vars",
                "fitted", "residuals")

  m$call <- match.call()
  m$vars <- as.character(m$call)[-1]
  
  m$data <- data.frame(x = x, y = y)
  names(m$data) <- m$vars
  
  # Calculate the covariance and variances
  cor_xy <- stats::cor(x, y)
  sd_x <- stats::sd(x)
  sd_y <- stats::sd(y)
  
  # Calculate the SMA slope()
  m$slope <- sign(cor_xy) * (sd_y / sd_x)
  
  # Calculate the intercept
  m$intercept <- mean(y) - m$slope * mean(x)

  # Calculate the fitted values and residuals
  m$fitted <- m$slope * x + m$intercept
  m$residuals <- y - m$fitted
  
  r <- cor_xy / (sd_x * sd_y)
  # Estimate summary statistic attributes for the model
  n <- length(m$residuals)
  
  # Residual standard error (Root Mean Square Error - RMSE)
  rmse <- sqrt(mean(m$residuals^2))
  
  # R squared calculation
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum(m$residuals^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Estimate Corrected AICc
  k <- 2  # Number of parameters (only ever slope & intercept)
  aic <- n * log(ss_residual / n) + 2 * k
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)
  
  # Create the model object
  model <- structure(m, 
                     class = "sma_model",
                     r_squared = r_squared,
                     rmse = rmse,
                     aicc = aicc, r= r
                     )
  
  return(model)
}

#' Plot method for the SMA model output
#'@param x An object of the class _sma_model_
#'@param ... Other parameters passed to plot.default
#'@rdname sma_model
#'@export

plot.sma_model <- function(x,...){
  
  mod_data <- x$data
  
  plot(x = mod_data[,1], y = mod_data[,2], ...)
  
  abline(a= x$intercept, b = x$slope, col= "green", lwd = 2)
}
