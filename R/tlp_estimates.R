#' Estimate the RWC and RWD at turgor loss point for using the relationship between leaf pressure potential and bulk and symplastic relative water deficit
#'
#' @param psip A vector of pressure potential values
#' @param rwd A vector of relative water deficit values
#' @param symrwd A vector of symplastic relative water deficit values
#'
#' @return Bulk and symplastic slopes and intercepts.

psip_rwd_params <- function(psip, rwd, symrwd) {
  # create an input class for the data
  tlp_input <- tlpinput(psip, rwd, symrwd)
  # construct bulk and symplastic models
  tlp_model <- sma_model(tlp_input)
  # return the model parameters
  invisible(tlp_model)
}

#' Constructor for the tlp_input class
tlpinput <- function(psip, rwd, sym_rwd) {
  input <- structure(list(psip, rwd, sym_rwd),
                     .Names=c("psip", "rwd", "sym_rwd"),
            class = "tlp_input")
  return(input)
}


#' Estimate pressure volume curve parameters at turgor loss
#' 
#' @description Estimate leaf water potential, relative water content at turgor loss point and modulus of elasticity both bulk parameters and their symplastic counterparts. 
#'
#' Two methods are provided for this function: \code{estTLP.default} and \code{estTLP.osmEst}. The default method requires a data frame with columns for pressure potential and relative water content. The osmEst method requires an object of class _osmEst_ (i.e., produced by the estOsmotic function). The latter is recommended for simplicity and consistency with other functions in the package. 
#'
#' @details The function estimates the standard major axis model parameters **above** turgor loss between pressure potential and the bulk and symplastic relative water deficit to estimate the bulk and symplastic relative water content at turgor loss point as : 
#' \deqn{RWD_{TLP} = -\frac{SMA~Intercept}{SMA~Slope}}
#' \deqn{RWC_{TLP} = 100 - RWD_{TLP}}
#' \deqn{Symplastic~RWD_{TLP} = -\frac{SMA~Intercept_{sym}}{SMA~Slope_{sym}}}
#' \deqn{Symplastic~RWC_{TLP} = 100 - Symplastic~RWD_{TLP}}
#' Then, the water potential at turgor loss point is calculated using the SMA model parameters for hydration states **below** turgor loss point as:
#' \deqn{\pi_{TLP} = -\frac{1}{Slope_{osm} \times RWD_{TLP} + Intercept_{osm}}}
#' Then, it calculates the bulk and symplastic modulus of elasticity as the ratio of the pressure potential at full turgor and the relative water content at turgor loss point:
#' \deqn{\epsilon_{TLP} = \frac{P_{0}}{RWC_{TLP}}}
#' \deqn{Symplastic~\epsilon_{TLP} = \frac{P_{0}}{Symplastic~RWC_{TLP}}}
#'
#' @param data A data frame
#' @param wc.index Index of relative water content values
#' @param wp.index Index of water potential values.
#' @param n_row_above Number of rows above turgor loss point guess for estimating SMA line parameters.
#' @param n_row_below Number of rows below turgor loss point guess for estimating SMA line parameters.
#'
#' @return Returns an object of class _tlpEst_ with the following components:
#' \item{pi_tlp}{Osmotic potential at turgor loss point}
#' \item{rwc_tlp}{Bulk relative water content at turgor loss point}
#' \item{rwd_tlp}{Bulk relative water deficit at turgor loss point}
#' \item{sym_rwc_tlp}{Symplastic relative water content at turgor loss point}
#' \item{sym_rwd_tlp}{Symplastic relative water deficit at turgor loss point}
#' \item{modulus}{Bulk modulus of elasticity at turgor loss point}
#' \item{sym_modulus}{Symplastic modulus of elasticity at turgor loss point}
#' 
#' @importFrom dplyr arrange slice_tail slice_head
#' @export
#' @rdname estTLP

estTLP <- function(data, wc.index, wp.index, n_row_above, n_row_below, ...){
  UseMethod("estTLP")
}

#' @export
#' @rdname estTLP
estTLP.default <- function(data, wc.index, wp.index, n_row_above, n_row_below) {

  osm_obj <- estOsmotic(data = data, wc.index, wp.index, n_row = n_row_below)
  
  above_idx <- c(1:n_row_above)
  
  param_list <- psip_rwd_params(
    psip = osm_obj$prespot[above_idx],
    rwd = osm_obj$data$rwd[above_idx], 
    symrwd = osm_obj$symrwd[above_idx]
  ) 
  
  #collect model parameters for below from osm object
  osm_slope <- osm_obj$model$slope
  osm_intercept <- osm_obj$model$intercept
  
  # calculate PV parameters at turgor loss point
  rwd_tlp <- -((param_list$intercept) / (param_list$slope))
  rwc_tlp <- 100 - rwd_tlp
  sym_rwd_tlp <- -((param_list$intercept_sym) / (param_list$slope_sym))
  sym_rwc_tlp <- 100 - sym_rwd_tlp
  pi_tlp <- -1 / (osm_slope * rwd_tlp + osm_intercept)
  modulus <- osm_obj$psip_o / (rwd_tlp / 100)
  sym_modulus <- osm_obj$psip_o / (sym_rwd_tlp / 100)
  
  outtlp <- structure(list(
    pi_tlp, rwc_tlp, rwd_tlp, 
    sym_rwc_tlp, sym_rwd_tlp, 
    modulus, sym_modulus
  ), 
  .Names = c("pi_tlp", "rwc_tlp", "rwd_tlp", 
             "sym_rwc_tlp", "sym_rwd_tlp", 
             "modulus", "sym_modulus"),
  units = c("MPa", "%","%",
            "%","%", 
            "MPa", "MPa"),
  class= "tlpEst")
  
  return(outtlp)
}

NULL


#' @export
#' @rdname estTLP
estTLP.osmEst <- function(osm_obj, n_row_below = NULL, n_row_above = 4) {
  
  above_idx <- c(1:n_row_above)
  
  param_list <- psip_rwd_params(
    psip = osm_obj$prespot[above_idx],
    rwd = osm_obj$data$rwd[above_idx], 
    symrwd = osm_obj$symrwd[above_idx]
  ) 

  #collect model parameters for below from osm object
  osm_slope <- osm_obj$model$slope
  osm_intercept <- osm_obj$model$intercept

  # calculate PV parameters at turgor loss point
  rwd_tlp <- -((param_list$intercept) / (param_list$slope))
  rwc_tlp <- 100 - rwd_tlp
  sym_rwd_tlp <- -((param_list$intercept_sym) / (param_list$slope_sym))
  sym_rwc_tlp <- 100 - sym_rwd_tlp
  pi_tlp <- -1 / (osm_slope * rwd_tlp + osm_intercept)
  modulus <- osm_obj$psip_o / (rwd_tlp / 100)
  sym_modulus<- osm_obj$psip_o  / (sym_rwd_tlp / 100)
  
  outtlp <- structure(list(
    pi_tlp, rwc_tlp, rwd_tlp, 
    sym_rwc_tlp, sym_rwd_tlp, 
    modulus, sym_modulus
    ), 
    .Names = c("pi_tlp", "rwc_tlp", "rwd_tlp", 
               "sym_rwc_tlp", "sym_rwd_tlp", 
               "modulus", "sym_modulus"),
    units = c("MPa", "%","%",
              "%","%", 
              "MPa", "MPa"),
    class= "tlpEst")
  
  return(outtlp)
}


#' @export
print.tlpEst <- function(x, ...) {
  
  # get units
  units <- attr(x, "units")
  
  cat("Estimated PV parameters at turgor loss point (TLP):\n")
  cat("----------------------------------------------------\n")
  cat("Osmotic potential at TLP:  ", x$pi_tlp|>round(3), units[1], "\n")
  cat("Bulk RWC at TLP:  ", x$rwc_tlp|>round(3), units[2], "\n")
  cat("Bulk RWD at TLP:  ", x$rwd_tlp|>round(3), units[3], "\n")
  cat("Symplastic RWC at TLP:  ", x$sym_rwc_tlp|>round(3), units[4], "\n")
  cat("Symplastic RWD at TLP:  ", x$sym_rwd_tlp|>round(3), units[5], "\n")
  cat("Bulk modulus at TLP:  ", x$modulus|>round(3), units[6], "\n")
  cat("Symplastic modulus at TLP:  ", x$sym_modulus|>round(3), units[7], "\n")
  cat("----------------------------------------------------\n")
}

