# Contains functions for estimating the leaf osmotic parameters from the dataset with saturated and relative water content
# already determined.


#' Osmotic potential at full turgor estimate
#'
#' @description Estimates the osmotic potential at full turgor from the linear relationship
#'     between relative water deficit and inverse leaf water potential.
#'
#' @param data A data frame. A data frame containing the data set of the last 4 hydration states for a given leaf
#' @param rwd A vector of relative water deficit values
#' @param psi A vector of water potential values
#'
#' @return Numeric list containing the slope, intercept and osmotic potential at full turgor, in that order. Class "osmEst"
#'
#' @export
#'
#' @seealso \code{\link{sma_slope}}, \code{\link{sma_intercept}}
#'

estpio <- function(rwd, psi) {

  stopifnot(length(rwd)==length(psi))
  
  input_vals <- osminput(rwd, psi)
  
  osm_mod <- sma_model(x=input_vals)
  
  pio <- -1 / osm_mod$intercept

  output <- structure(list(
    "sma_mod" = osm_mod,
    "pio" = pio
  ),
  class = "pioEst")

  return(output)
}


#' Constructor for osmotic inputs
#'
#' @param rwc Values of relative water content and deficit
#' @param psi Values of water potential 
#' 
osminput <- function(rwd, psi) {

  inputs <- structure(list(rwd, psi),
                      .Names =  c("rwd", "neg_inv_psi"),
                      class = "osm_input")

  return(inputs)
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
#' @param silent Logical. Silences messages if TRUE
#' @details
#' This function estimates osmotic variables from the values \strong{below} turgor loss point. Here we assume that the data points at
#' last 4 hydration states all represent points below turgor loss point.
#'
#' It is implemented \strong{after} estimation of leaf saturated water content, relative water content and relative
#' water deficit.See `estRWC` for information on the estimation of those parameters.
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
#' @seealso [estRWC()], [sma_intercept()]

estOsmotic <- function(data, wc.index, wp.index, n_row = 4, silent=T,...){
  UseMethod("estOsmotic")
}

#'@export
estOsmotic.default <- function(data, wc.index, wp.index, n_row = 4, silent=T) {
  
  data_belowtlp <- data %>%
    dplyr::arrange(desc(wp.index)) %>%
    dplyr::slice_tail(n = n_row) %>%
    as.data.frame()
  
  is_char <- all(is.character(c(wc.index,wp.index)))
  is_num <- all(is.numeric(c(wc.index,wp.index)))
  
  if (!(is_char|is_num)) {
    stop("estOsmotic: Column indices must both be either character strings or numeric integers referencing the preferred column.")
  }
  
  if(is_char){
    varnames <- list(
      "wc" = wc.index,
      "wp" = wp.index
    )
  }else if(is_num){
    varnames <- list(
      "wc" = names(data)[wc.index],
      "wp" = names(data)[wp.index]
    )
  }
  
  check_var <- all(varnames[c("wc", "wp")] %in% names(data))
  
  if (check_var == FALSE) {
    stop("estOsmotic: The column names provided do not exist in the input data.")
  }
  
  if (silent == FALSE) {
    cat("\nEstimating osmotic variables...\n\n")
    
    print(head(data))

    cat("Using the following columns for the estimation:\n",
      "{RWC/RWD}: ", varnames$wc, "\n",
      "{Water potential}: ", varnames$wp, "\n\n",
      sep = ""
    )
    
  }
  # create vectors
  rwc <- data_belowtlp[[wc.index]]
  rwd <- 100 - rwc
  psi <- data_belowtlp[[wp.index]]
  minus_inv_psi <- -1/psi

  pio <- estpio(rwd, minus_inv_psi)
  
  #calculate osmotic and pressure potential at full turgor
  osm_pot_fullturgor <- pio$pio
  max_psip <- osm_pot_fullturgor * -1

  osmotic_potential <- -1 / (pio$sma_mod$intercept + (pio$sma_mod$slope * rwd))
  pressure_potential <- psi - osmotic_potential
  apoplastic_fraction <- 100 + (pio$sma_mod$intercept / pio$sma_mod$slope)
  sym_rwc <- (rwc - apoplastic_fraction)/(100 - apoplastic_fraction)
  sym_rwd <- 100 - sym_rwc

  structure(list("psi" = psi, 
                 "invpsi" = minus_inv_psi,
                 "pio" = osm_pot_fullturgor,
                 "psip_o"= max_psip,
                 "osmpot" = osmotic_potential,
                 "prespot" = pressure_potential, 
                 "af" = apoplastic_fraction, 
                 "symrwc" = sym_rwc, 
                 "symrwd" = sym_rwd,
                 "data" = data, # bring the data and model estimates along
                 "model" = pio$sma_mod), 
                 units = c("MPa","-MPa^-1", "MPa", "MPa","MPa", "MPa" ,"%", "%", "%"),
                 class="osmEst")
}


# Print method of osmEst class objects
#'@export
print.osmEst <- function (x, ...){

  cat("Osmotic potential at full turgor:", x$pio, "\n")
  cat("Pressure potential at full turgor:", x$psip_o, "\n")
  cat("Apoplastic fraction:", x$af, "\n")
  
}