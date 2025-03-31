#' Standard major axis model fitting
#' @description
#' This function calculates the standard major axis model (SMA) parameters for
#' @details
#'  This generic function will dispatch 1 of 2 methods based on the input data type (i.e.,vectors or an object of class _`osm_input`_) to construct a standard major axis model from calculated slope and intercept for a given dependent and independent variable:
#' * _default_ : slope and intercept are estimated from input _x_ and _y_ vectors
#' * _`osm_input`_: Input is an object of class _`osm_input`_. Model parameters will be estimated from the negative inverse of water potential and relative water deficit.
#' * The SMA model is also used for determining relative water content at turgor loss and capacitance at full turgor and turgor loss in [psip_rwd_params()] from the slopes of fitted models.
#'
#' The SMA slope (\eqn{\beta_{SMA}}) is calculated as:
#'
#' \deqn{\beta_{SMA} = \text{sign} (r) \times \frac{\sigma_y}{\sigma_x}}
#'
#' where \eqn{r} is the correlation of x and y, and \eqn{\sigma_y} and \eqn{\sigma_x} are the standard deviations of y and x, respectively.
#'
#' Then, the SMA intercept (\eqn{\alpha_{SMA}}) is calculated as:
#'
#' \deqn{\alpha_{SMA} = \bar{y} - \beta_{SMA} * \bar{x}}
#'
#' where \eqn{\bar{x}}, \eqn{\bar{y}} are means.
#'
#'
#' @param x Independent variable
#' @param y Dependent variable
#' @param ... additional parameters passed to methods
#'
#' @returns Fitted model parameters (i.e., slope and intercept), fitted values, residuals, as well as model fit metrics as model attributes:
#' \item{slope}{The SMA slope (\eqn{\beta_{SMA}})}
#' \item{intercept}{The SMA intercept (\eqn{\alpha_{SMA}})}
#' \item{fitted}{Values fitted using model parameters}
#' \item{residuals}{Residuals of fitted values}
#' \item{model attributes}{The coefficient of determination (\eqn{R^{2}}), root mean square error (rmse), and corrected Aikaike Information Criterion (\eqn{AIC_{C}}). See `attributes(fitted_model)`. }
#'
#' @export
#' @rdname sma_model
#'
#' @seealso [estOsmotic()], [estTLP()], [psip_rwd_params()]
#' @references
#' Warton DI, Wright IJ, Falster DS, Westoby M. (2006) Bivariate line-fitting methods for allometry. *Biological Reviews*. 81: 259-291. DOI: \url{https://doi.org/10.1017/S1464793106007007}
#'
#' Jolicoeur P (1990) Bivariate allometry: Interval estimation of the slopes of the ordinary and standardized normal major axes and structural relationship. *Journal of Theoretical Biology* 144: 275–285. DOI: \url{https://doi.org/10.1016/S0022-5193(05)80326-1}

sma_model <- function(x, y) {
  UseMethod("sma_model")
}

#' @rdname sma_model
#' @export
sma_model.default <- function(x, y) {
  # Check input data
  if (any(is.na(x)) | any(is.na(y))) {
    stop(
      "sma_model:Missing values found in input data. Ensure that there are no missing values."
    )
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric vectors.")
  }
  if (length(x) != length(y)) {
    stop("x and y must have the same length.")
  }
  
  # establish model object
  m <- vector(mode = "list", length = 6)
  names(m) <- c("slope", "intercept", "data", "call", "fitted", "residuals")
  
  m$call <- match.call()
  
  m$data <- data.frame(x = x, y = y)
  names(m$data) <-  as.character(m$call)[-1] # set names of the data df columns
  
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
  
  #r <- cor_xy / (sd_x * sd_y)
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
  model <- structure(
    m,
    class = "sma_model",
    r_squared = r_squared,
    rmse = rmse,
    aicc = aicc
  )
  
  return(model)
}

#' @rdname sma_model
#' @export
sma_model.osm_input <- function(x, y = NULL) {
  if (any(is.na(x[[1]])) | any(is.na(x[[2]]))) {
    stop(
      "sma_model:Missing values found in input data. Ensure that there are no missing values."
    )
  }
  
  model <- pvest::sma_model(x = x$neg_inv_psi, y = x$rwd)
  
  invisible(model)
  
}


#' Calculate the slopes for turgor loss inputs
#' @description
#' This function estimates the standard major axis parameters used in predicting values at water potential at turgor loss
#' @seealso [estTLP()]

#' @param x Object of class _`tlp_input`_
#' @returns Calculated slopes and intercepts relating pressure potential with relative water deficit, and relative water content with water potential above and below turgor loss point. See attr(x, "model_list") for explicit display of fitted models and data used.

calc_param_tlp <- function(x, y = NULL, ...) {
  if (any(is.na(unlist(x, recursive = TRUE)))) {
    stop(
      "calc_param_tlp:Missing values found in input data. Ensure that there are no missing values."
    )
  }
  
  bulk_psip_rwd <- pvest::sma_model(x$psip, x$rwd)
  sym_psip_rwd <- pvest::sma_model(x$psip, x$symrwd)
  
  # above tlp
  bulk_rwc_psi_above <- pvest::sma_model(x$rwc_above, x$psi_above)
  sym_rwc_psi_above <- pvest::sma_model(x$symrwc_above, x$psi_above)
  
  # below tlp
  bulk_rwc_psi_below <- pvest::sma_slope(x$rwc_below, x$psi_below)
  sym_rwc_psi_below <- pvest::sma_slope(x$symrwc_below, x$psi_below)
  
  # extract relevant model parameters for rwc and cap
  params <- structure(
    list(
      slope = bulk_psip_rwd$slope,
      intercept = bulk_psip_rwd$intercept,
      slope_sym = sym_psip_rwd$slope,
      intercept_sym = sym_psip_rwd$intercept,
      slope_cap_ft = bulk_rwc_psi_above$slope * 0.01,
      slope_cap_sym_ft = sym_rwc_psi_above$slope * 0.01,
      slope_cap_tlp = bulk_rwc_psi_below$slope * 0.01,
      slope_cap_sym_tlp = sym_rwc_psi_below$slope * 0.01
    )
  )
  # return fitted models as attribute to parameter list
  attr(params, "model_list") <- list(
    bulk_rwctlp = bulk_psip_rwd,
    sym_rwctlp = sym_psip_rwd,
    bulk_capft = bulk_rwc_psi_above,
    sym_capft = sym_rwc_psi_above,
    bulk_captlp = bulk_rwc_psi_below,
    sym_captlp = sym_rwc_psi_below
  )
  
  invisible(params)
}

#' Print method for the SMA model output

#' @importFrom withr local_options
#' @return Printed SMA model output
#' @export print.sma_model

print.sma_model <- function(x, ...) {
  withr::local_options(list(digits = 4))
  
  cat("Standard Major Axis (SMA) model\n")
  cat("-------------------------------")
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  cat("Slope:     ", x$slope, "\n")
  cat("Intercept: ", x$intercept, "\n")
  cat("\nModel Fit Metrics:\n")
  cat("Residual Standard Error (RMSE):", attr(x, "rmse"), "\n")
  cat("R-squared:                     ", attr(x, "r_squared"), "\n")
  cat("AICc:                          ", attr(x, "aicc"), "\n")
}

#' Plot method for the SMA model output
#'@param ... Other parameters passed to print and plot methods
#'@rdname sma_model
#'@export

plot.sma_model <- function(x, ...) {
  mod_data <- x$data
  
  plot(x = mod_data[, 1], y = mod_data[, 2], ...)
  
  abline(
    a = x$intercept,
    b = x$slope,
    col = "green",
    lwd = 2.5
  )
}
