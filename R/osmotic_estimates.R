#' Estimate symplastic water content
#' 
#' @param rwc Vector of relative water content values.
#' @param sma_mod An object of class "sma_model" containing the slope and intercept of the linear relationship between relative water deficit and negative inverse water potential.
#' @return A vector of symplastic relative water content values.
#' 
sym_rwc <- \(rwc, sma_mod) {
  # apoplastic fraction
  # af is the x intercept of the relationship between rwd and negative inverse water potential to put it into terms of relative water content ADD 100
  af <- (sma_mod$intercept / sma_mod$slope) + 100
  
  # symplastic relative water content
  srwc_num <- rwc/ 100 - af / 100
  srwc_den <- 1 - af / 100
  srwc <- srwc_num / srwc_den
  
  return(list(srwc = srwc, 
              srwd = 1-srwc, 
              af = af))
}

#' Osmotic potential at full turgor estimate
#'
#' @description Estimates the osmotic potential at full turgor from the linear relationship
#'     between relative water deficit and inverse leaf water potential.
#'
#' @param rwd Vector of relative water deficit values.
#' @param psi Vector of negative inverse of water potential values.
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
  inputs <- structure(
    list(rwd, psi),
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
#' @importFrom dplyr mutate
#' @rdname estOsmotic

estOsmotic <- function(data, ...) {
  UseMethod("estOsmotic")
}

#' @rdname estOsmotic
#' @export
estOsmotic.default <- function(
  data,
  wc.index,
  wp.index,
  n_row = 4,
  silent = T,
  ...
) {
  is_char <- all(c(is.character(wc.index), is.character(wp.index)))
  is_num <- all(c(is.numeric(wc.index), is.numeric(wp.index)))

 if (!(is_char | is_num)) {
    stop(
      "estOsmotic: Column indices must both be either character strings or numeric integers referencing the preferred column."
    )
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
    stop(
      "estOsmotic: The column names provided do not exist in the input data."
    )
  }

  if (silent == FALSE) {
    cat("\nEstimating osmotic variables...\n\n")

    print(head(data))

    cat(
      "Using the following columns for the estimation:\n",
      "{RWC/RWD}: ",
      varnames$wc,
      "\n",
      "{Water potential}: ",
      varnames$wp,
      "\n\n",
      sep = ""
    )
  }

  # create output
  rwd_n <- 100-tail(data[[varnames$wc]], n = n_row)
  psi_n <- tail(data[[varnames$wp]], n = n_row)
  minus_inv_psi <- -1 / psi_n

  pio <- estpio(rwd_n, minus_inv_psi)

  # calculate osmotic and pressure potential at full turgor
  osm_pot_fullturgor <- pio$pio
  max_psip <- osm_pot_fullturgor * -1
  
  # calculate symplastic relative water content and apoplastic fraction
  sym_af <- sym_rwc(data[[varnames$wc]], pio$sma_mod)
  
  apoplastic_fraction <- sym_af$af #|> rep(x = _, nrow(data))
  sym_rwc <- sym_af$srwc #* 100
  sym_rwd <- sym_af$srwd #* 100

  osmotic_potential <- osm_pot_fullturgor / sym_af$srwc

  # calculate nonlinear pressure potential parameters
  pressure_potential_lin <- data[[wp.index]] - osmotic_potential

  rtlp_mod <- sma_model(
    head(sym_rwc, n = nrow(data) - n_row),
    head(pressure_potential_lin, n = nrow(data) - n_row)
  )

  r_tlp_init <- -rtlp_mod$intercept / rtlp_mod$slope

  psip_mod <- calc_nonlin_psip(
    data = data.frame(r = sym_rwc, psip_linear = pressure_potential_lin),
    pi_sat = osm_pot_fullturgor,
    r_tlp = r_tlp_init
  )

  pressure_potential <- fitted(psip_mod) |> as.vector()

  dataUpd <- data |> 
    dplyr::mutate(
      invpsi = -1 / data[[varnames$wp]],
      pio = osm_pot_fullturgor,
      psip_o = max_psip,
      osmpot = osmotic_potential,
      prespot = pressure_potential,
      af = apoplastic_fraction,
      symrwc = sym_rwc,
      symrwd = sym_rwd
    )
  
  structure(
    list(
      "psi" = data[[varnames$wp]],
      "invpsi" = -1 / data[[varnames$wp]],
      "pio" = osm_pot_fullturgor,
      "psip_o" = max_psip,
      "osmpot" = osmotic_potential,
      "prespot" = pressure_potential,
      "af" = apoplastic_fraction,
      "symrwc" = sym_rwc,
      "symrwd" = sym_rwd,
      "rwctlp" = r_tlp_init, 
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
    cat(
      "Using the following columns for the estimation:\n",
      "{RWC/RWD}: ",
      obj_names[3],
      "\n",
      "{Water potential}: psi",
      "\n\n",
      sep = ""
    )
  }

  # create output
  rwd_n <- tail(osm_obj$rwd, n = n_row)
  psi_n <- tail(rwcEstData$water.potential, n = n_row)
  minus_inv_psi <- -1 / psi_n

  pio <- estpio(rwd_n, minus_inv_psi)

  # calculate osmotic and pressure potential at full turgor
  osm_pot_fullturgor <- pio$pio
  max_psip <- osm_pot_fullturgor * -1

  #calculate symplastic relative water content and apoplastic fraction
  sym_af <- sym_rwc(osm_obj$rwc, pio$sma_mod)

  apoplastic_fraction <- sym_af$af
  sym_rwc <- sym_af$srwc
  sym_rwd <- sym_af$srwd

  osmotic_potential <- osm_pot_fullturgor / sym_af$srwc

  # calculate nonlinear pressure potential parameters
  pressure_potential_lin <- rwcEstData$water.potential - osmotic_potential

  rtlp_mod <- sma_model(
    head(sym_rwc, n = nrow(rwcEstData) - n_row),
    head(pressure_potential_lin, n = nrow(rwcEstData) - n_row)
  )

  r_tlp_init <- -rtlp_mod$intercept / rtlp_mod$slope
  
  psip_mod <- calc_nonlin_psip(
    data = data.frame(r = sym_rwc, psip_linear = pressure_potential_lin),
    pi_sat = osm_pot_fullturgor,
    r_tlp = r_tlp_init
  )

  pressure_potential <- fitted(psip_mod)|> as.vector()

  dataUpd <- rwcEstData |> 
    dplyr::mutate(
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

#' Nonlinear pressure potential estimate
#' @description Estimate the pressure potential using a nonlinear model
#'
#' @param data A data frame. A data frame containing the data set of the hydration states for a given leaf above turgor loss estimate
#' @param pi_sat A double. Osmotic potential at full turgor
#' @param r_tlp A double. Relative water content at turgor loss point
#' @param psi_w A double. Leaf water potential (optional, used if full = TRUE)
#' @param full Logical. If TRUE, fits the full model including water potential, otherwise fits only the pressure potential model
#' @details Here we fit a nonlinear model to estimate the pressure potential based on the osmotic potential at full turgor where the pressure potential decreases nonlinearly before turgor loss with a slope of b :
#' Given that: 
#' \deqn{\Psi_{p,max} = \pi_{o} \cdot -1}
#' 
#' then, 
#' 
#' \deqn{\Psi_p = \Psi_{p,max} \cdot (\frac{RWC_{sym} - RWC_{TLP}}{1 - RWC_{TLP}})^b}
#' 
#' @returns Pressure potential estimates
#'
#' @importFrom minpack.lm nlsLM nls.lm.control
#'
#' @export

calc_nonlin_psip <- function(data, pi_sat, r_tlp, psi_w = NULL, full = FALSE) {
  if (full == FALSE) {
    fit <- minpack.lm::nlsLM(
      # Model equation
      psip_linear ~
        ifelse(r > r_tlp, -pi_sat * ((r - r_tlp) / (1 - r_tlp))^(b), 0),
      # Data frame
      data = data,
      start = list(b = 2),
      # Lower bounds
      lower = c(b = 0),
      # Upper bounds
      upper = c(b = 10),

      control = minpack.lm::nls.lm.control(maxiter = 100)
    )
  } else {
    tryCatch(
      {
        fit <- minpack.lm::nlsLM(
          psi_w ~
            (pi_sat / r) +
              ifelse(r > r_tlp,
                     -pi_sat * ((r - r_tlp) / (1 - r_tlp))^(b), 0),
          # Model equation
          data = data,
          # Data frame
          start = list(b = 2),
          # Lower bounds
          lower = c(b = 0),
          # Upper bounds
          upper = c(b = 10),
          # Upper bounds
          control = minpack.lm::nls.lm.control(maxiter = 100)
        )
      },
      error = function(e) {
        message("Non-linear model fitting failed: ", e$message)
        return(NULL)
      }
    )
  }
  return(fit)
}

#' Print method of osmEst class objects
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
