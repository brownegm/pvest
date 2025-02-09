# Contains functions for estimating the leaf osmotic parameters from the dataset with saturated and relative water content
# already determined.


#' Osmotic potential at full turgor estimate
#'
#' @description Estimates the osmotic potential at full turgor from the linear relationship
#'     between relative water deficit and inverse leaf water potential.
#'
#' @param data A data frame. A data frame containing the data set of the last 4 hydration states for a given leaf
#' @param rwc A vector of relative water content or deficit data
#' @param psi A vector of water potential data
#'
#' @return Numeric list containing the slope, intercept and osmotic potential at full turgor, in that order. Class "osmEst"
#'
#' @export
#'
#' @seealso \code{\link{sma_slope}}, \code{\link{sma_intercept}}
#'

estpio <- function(rwc, psi) {

  stopifnot(length(rwc)==length(psi))
  
  class(rwc) <- "osm_input"
  class(psi) <- "osm_input"
  
  #slope is negative here
  osm_mod <- sma_model(x = psi, 
                       y = rwc)
  
  pi.o <- -1 / osm_mod$intercept

  output <- structure(list(
    # "slope" = osm_mod$slope,
    # "intercept" = osm_mod$intercept,
    "sma_mod" = osm_mod,
    "pi.o" = pi.o
  ),
  class = "pioEst")

  return(output)
}

#' Print method for the Osmotic estimates output
#'
#' @param x An object of the class "pioEst"
#' @param ... Other parameters passed to cat
#'
#' @returns Printed SMA model output
#' @export print.pioEst

print.pioEst <- function(x, ...){
  cat("Pi estimates: \n")|>eval()
  cat("Osmotic potential at full turgor:", x$pi.o, "\n")
  return(x$sma_mod)
}
NULL

#' Osmotic and pressure parameter estimation
#' @description Estimate the osmotic potential at full turgor, pressure potential at full turgor for leaves
#'     as well as the osmotic potential and pressure potential for each hydration state.
#'
#' @param data A data frame. A data frame containing the data set of the last 4 hydration states for a given leaf
#' @param wc.index A double. A data frame index which contains the water content data passed through OsmoticPotFullTurgor function
#' @param wp.index A double. A data frame index which contains the leaf water potential data
#' @param n_row A double. Value which indicates the number of rows for estimating parameters. Default to 4 rows.
#'
#' @details
#' This function estimates osmotic variables from the values \strong{below} turgor loss point. Here we assume that the data points at
#' last 4 hydration states all represent points below turgor loss point.
#'
#' It is implemented \strong{after} estimation of leaf saturated water content, relative water content and relative
#' water deficit.See `RelativeWaterCD` for information on the estimation of those parameters.
#'
#' Here, we estimate the osmotic potential at full turgor as the x-intercept of the relationship between
#' inverse leaf water potential and relative water deficit below turgor loss point. The slope needs to be negative
#' within the sma_intercept function(\code{\link{sma_intercept}}) because of the negative inverse of leaf water potential.
#' Also, note that unlike in the saturated water content estimation the x values is inverse.
#'
#'
#' @return Returns data frame with new columns containing the osmotic and pressure potential variables namely:
#'    \itemize{
#'     \item osmotic potential at full turgor
#'     \item maximum pressure potential
#'     \item osmotic potential (for each hydration state)
#'     \item pressure potential (for each hydration state)
#'     \item apoplastic fraction
#'     \item symplastic relative water content(sym.rwc; for each hydration state)
#'     }
#'
#' @import dplyr
#' @export estOsmotic
#' @seealso [RelativeWaterCD()], [sma_intercept()]

estOsmotic <- function(data, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row = 4) {
  
  data_belowtlp <- data %>%
    dplyr::arrange(desc(wp.index)) %>%
    dplyr::slice_tail(n = n_row) %>%
    as.data.frame()
  
  # create vectors
  rwc <- data_belowtlp[["wc.index"]]
  rwd <- 100 - rwc
  psi <- data_belowtlp[["wp.index"]]
  
  minus_inv_psi <- -1/psi

  pio <- estpio (rwc, minus_inv_psi)
  
  #calculate osmotic and pressure potential at full turgor
  osm.pot.fullturgor <- pio$pi.o
  max.psip <- osm.pot.fullturgor * -1

  osmotic_potential <- -1 / (pio$model$intercept + (pio$model$intercept * rwd))
  pressure_potential <- psi - osmotic_potential
  apoplastic_fraction <- 100 + (pio$model$intercept / pio$model$slope)
  sym_rwc <- ((rwc - (apoplastic_fraction)) / (100 - (apoplastic_fraction))) * 100
  sym_rwd <- 100 - sym_rwc

  structure(list("psi" = psi, 
                 "pio" = osm.pot.fullturgor,
                 "psip_o"= max.psip,
                 "osmpot" = osmotic.potential,
                 "prespot" = pressure_potential, 
                 "af" = apoplastic_fraction, 
                 "symrwc" = sym_rwc, 
                 "symrwd" = sym_rwd), 
                 class="osmEst")
}

