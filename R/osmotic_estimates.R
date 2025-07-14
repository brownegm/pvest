#' Osmotic potential at full turgor estimate
#'
#' @description Estimates the osmotic potential at full turgor from the linear relationship
#'     between relative water deficit and inverse leaf water potential.
#'
#' @param rwd Vector of relative water deficit values.
#' @param psi Vector of water potential values.
#'
#' @return An object of class "pioEst" containing the slope, intercept and osmotic potential at full turgor. 
#'
#' @export
#'
#' @seealso [sma_model()]
#'

estpio <- function(rwd, psi) {
  stopifnot(length(rwd) == length(psi))

  input_vals <- osminput(rwd, psi)

  osm_mod <- sma_model(x = input_vals)

  pio <- -1 / osm_mod$intercept

  output <- structure(
    list(
      "sma_mod" = osm_mod,
      "pio" = pio
    ),
    class = "pioEst"
  )

  return(output)
}


#' Constructor for osmotic inputs
#'
#' @param rwd Values of relative water content and deficit
#' @param psi Values of water potential

osminput <- function(rwd, psi) {
  inputs <- structure(list(rwd, psi),
    .Names = c("rwd", "neg_inv_psi"),
    class = "osm_input"
  )

  return(inputs)
}

#' Print method for the Osmotic estimates output
#'
#' @param x An object of the class "pioEst"
#' @param ... Other parameters passed to cat
#'
#' @returns Printed SMA model output
#' @export print.pioEst

print.pioEst <- function(x, ...) {
  cat("Pi estimates: \n")
  cat("Osmotic potential at full turgor:", x$pi.o, "\n")
  return(x$sma_mod)
}
NULL

#' Osmotic and pressure parameter estimation
#' @description Estimate the osmotic potential at full turgor, pressure potential at full turgor for leaves
#'     as well as the osmotic potential and pressure potential for each hydration state.
#'
#' @param data A data frame. A data frame containing the data set of the last 4 hydration states for a given leaf
#' @param wc.index A double. A data frame index which contains the water content (RWD) data passed to estpio()
#' @param wp.index A double. A data frame index which contains the leaf water potential data
#' @param n_row A double. Value which indicates the number of rows for estimating parameters. Default to 4 rows.
#' @param silent Logical. Silences messages if TRUE
#' @param ... Additional parameters passed to method (unused)
#'
#' @details
#' This function estimates osmotic variables from the values \strong{below} turgor loss point. Here we assume that the data points at
#' last 4 hydration states all represent points below turgor loss point.
#'
#' It is implemented \strong{after} estimation of leaf saturated water content, relative water content and relative water deficit.See `estRWC` for information on the estimation of those parameters.
#'
#' Here, we estimate the osmotic potential at full turgor as the x-intercept of the relationship between
#' inverse leaf water potential and relative water deficit below turgor loss point.
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
#' @export estOsmotic
#' @seealso [estRWC()], [sma_model()]
#' @importFrom utils head tail
#' @rdname estOsmotic

estOsmotic <- function(data, ...) {
  UseMethod("estOsmotic")
}

#' @rdname estOsmotic
#' @export
estOsmotic.default <- function(data, wc.index, wp.index, n_row = 4, silent = T, ...) {
  is_char <- all(c(is.character(wc.index), is.character(wp.index)))
  is_num <- all(c(is.numeric(wc.index), is.numeric(wp.index)))

  if (!(is_char | is_num)) {
    stop("estOsmotic: Column indices must both be either character strings or numeric integers referencing the preferred column.")
  }

  if (is_char) {
    varnames <- list(
      "wc" = wc.index,
      "wp" = wp.index
    )
  } else if (is_num) {
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
  rwd_n <- tail(data$rwd, n = n_row)
  psi_n <- tail(data$psi, n = n_row)
  minus_inv_psi <- -1 / psi_n

  pio <- estpio(rwd_n, minus_inv_psi)

  
  # calculate osmotic and pressure potential at full turgor
  osm_pot_fullturgor <- pio$pio
  max_psip <- osm_pot_fullturgor * -1

  osmotic_potential_old <- -1 / (pio$sma_mod$intercept + (pio$sma_mod$slope * (100 - data[[varnames$wc]])))
  
  r <- data[[varnames$wc]]/100
  r_tlp <- (pio$sma_mod$intercept / pio$sma_mod$slope) / 100
  # osmotic potential at full turgor
  osmotic_potential_new <- -pio * ((r - r_tlp)/(1 - r_tlp)) ^ b
  pressure_potential <- data[[varnames$wp]] - osmotic_potential
  apoplastic_fraction <- 100 + (pio$sma_mod$intercept / pio$sma_mod$slope)
  sym_rwc <- ((data[[varnames$wc]] - apoplastic_fraction) / (100 - apoplastic_fraction)) * 100
  sym_rwd <- 100 - sym_rwc

  dataUpd <- do.call(
    cbind,
    list(
      data,
      invpsi = -1 / data$psi,
      pio = osm_pot_fullturgor,
      psip_o = max_psip,
      osmpot = osmotic_potential,
      prespot = pressure_potential,
      af = apoplastic_fraction,
      symrwc = sym_rwc,
      symrwd = sym_rwd
    )
  )
  structure(
    list(
      "psi" = data$psi,
      "invpsi" = -1 / data$psi,
      "pio" = osm_pot_fullturgor,
      "psip_o" = max_psip,
      "osmpot" = osmotic_potential,
      "prespot" = pressure_potential,
      "af" = apoplastic_fraction,
      "symrwc" = sym_rwc,
      "symrwd" = sym_rwd,
      "data" = dataUpd, # bring the data and model estimates along
      "est_rows" = n_row,
      "model" = pio$sma_mod
    ),
    units = c("MPa", "-MPa^-1", "MPa", "MPa", "MPa", "MPa", "%", "%", "%"),
    class = "osmEst"
  )
}

#' @rdname estOsmotic
#' @param data An object of class "rwcEst"
#' @param n_row Number of rows above turgor loss
#' @param silent Silence printing of head(data) and columns used
#' @export
estOsmotic.rwcEst <- function(data, n_row = 4, silent = T, ...) {
  osm_obj <- data

  rwcEstData <- attr(osm_obj, "df")

  if (silent == FALSE) {
    cat("\nEstimating osmotic variables...\n\n")

    print(head(rwcEstData))

    obj_names <- names(osm_obj)
    cat("Using the following columns for the estimation:\n",
      "{RWC/RWD}: ", obj_names[c(3:4)], "\n",
      "{Water potential}: psi", "\n\n",
      sep = ""
    )
  }

  # create vectors
  rwd_n <- tail(osm_obj$rwd, n = n_row)
  psi_n <- tail(rwcEstData$water.potential, n = n_row)
  minus_inv_psi <- -1 / psi_n

  pio <- estpio(rwd_n, minus_inv_psi)

  # calculate osmotic and pressure potential at full turgor
  osm_pot_fullturgor <- pio$pio
  max_psip <- osm_pot_fullturgor * -1

  osmotic_potential <- -1 / (pio$sma_mod$intercept + (pio$sma_mod$slope * (100 - rwcEstData$rwc)))
  pressure_potential <- rwcEstData$water.potential - osmotic_potential
  apoplastic_fraction <- 100 + (pio$sma_mod$intercept / pio$sma_mod$slope)
  sym_rwc <- ((rwcEstData$rwc - apoplastic_fraction) / (100 - apoplastic_fraction)) * 100
  sym_rwd <- 100 - sym_rwc

  dataUpd <- do.call(
    cbind,
    list(
      rwcEstData,
      invpsi = -1 / rwcEstData$water.potential,
      pio = osm_pot_fullturgor,
      psip_o = max_psip,
      osmpot = osmotic_potential,
      prespot = pressure_potential,
      af = apoplastic_fraction,
      symrwc = sym_rwc,
      symrwd = sym_rwd
    )
  )

  osm <- structure(
    list(
      "psi" = rwcEstData$water.potential,
      "invpsi" = -1 / rwcEstData$water.potential,
      "pio" = osm_pot_fullturgor,
      "psip_o" = max_psip,
      "osmpot" = osmotic_potential,
      "prespot" = pressure_potential,
      "af" = apoplastic_fraction,
      "symrwc" = sym_rwc,
      "symrwd" = sym_rwd,
      "data" = dataUpd, # bring the data and model estimates along
      "est_rows" = n_row,
      "model" = pio$sma_mod
    ),
    units = c("MPa", "-MPa^-1", "MPa", "MPa", "MPa", "MPa", "%", "%", "%"),
    class = "osmEst"
  )

  return(osm)
}

# Print method of osmEst class objects
#' @export
print.osmEst <- function(x, ...) {
  cat("Osmotic and pressure potential estimates: \n")
  cat("-----------------------------------------------\n")
  cat("Osmotic potential at full turgor:", round(x$pio, 2), "MPa", "\n")
  cat("Pressure potential at full turgor:", round(x$psip_o, 2), "MPa", "\n")
  cat("Apoplastic fraction:", round(x$af, 2), "%", "\n")
  cat("Number of hydration states used:", x$est_rows, "\n")
  cat("-----------------------------------------------\n")
}
