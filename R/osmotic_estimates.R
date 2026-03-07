# estOsmotic generic and methods  -------------------------------------------

#' Osmotic and pressure parameter estimation
#' @description Estimate the osmotic potential at full turgor, pressure potential at full turgor for leaves
#'     as well as the osmotic potential and pressure potential for each hydration state.
#'
#' @param x A data frame. A data frame containing the data set of the last 4 hydration states for a given leaf
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
#' @importFrom stats predict coef
#' @importFrom cli cli_abort cli_warn
#' @rdname estOsmotic

estOsmotic <- function(x, ...) {
  UseMethod("estOsmotic")
}

#' @rdname estOsmotic
#' @export
estOsmotic.default <- function(
  x,
  wc.index,
  wp.index,
  n_row = 5,
  silent = T,
  ...
) {
  varnames <- get_varnames(data = x, wc.index = wc.index, wp.index = wp.index)

  if (silent == FALSE) {
    cat("\nEstimating osmotic variables...\n\n")

    print(head(x))

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

  n <- nrow(x)
  stopifnot(n_row >= 1L, n_row < n)

  # ---- Tail slices used for pio ----------------------------------------
  idx_tail <- (n - n_row + 1L):n
  rwd_tail <- x[[varnames$wc]][idx_tail]
  psi_tail <- x[[varnames$wp]][idx_tail]

  # validated input rwd values
  if (any(is.na(psi_tail))) {
    stop("Tail water potentials contain NA.")
  }
  if (any(psi_tail == 0)) {
    stop("Tail water potentials contain zeros (cannot invert).")
  }

  minus_inv_psi <- -1 / psi_tail

  # pio estimate (osmotic potential at full turgor)
  pio <- estpio(rwd_tail, minus_inv_psi)
  pi_sat <- pio$pio # osmotic potential at full turgor
  psip_sat <- -pi_sat

  # symplastic water content, osmotic, and pressure potential
  sym <- calc_symrwc(x$rwc, pio$sma_mod)
  af <- sym$af
  srwc <- sym$srwc
  srwd <- sym$srwd

  if (any(is.na(srwc) | srwc <= 0)) {
    stop("Saturated RWC must be positive.")
  }

  pi_vec <- pi_sat / srwc # osmotic potential
  psi_vec <- x[[varnames$wp]] # water potential
  psip_lin <- psi_vec - pi_vec # "linear" pressure potential

  # fit sma to points above tlp to get initial guess at tlp
  fit_len <- seq_len(n - n_row) #(n_row + 1))
  rtlp_fit <- sma_model(srwc[fit_len], psip_lin[fit_len])
  r_tlp_init <- -rtlp_fit$intercept / rtlp_fit$slope

  psip_mod <- calc_nonlin_psip(
    data = data.frame(r = srwc, psip_linear = psip_lin),
    pi_sat = pi_sat,
    r_tlp = r_tlp_init
  )
  if (is.null(psip_mod)) {
    cli::cli_abort(c(
      "Non-linear pressure potential model returned {.val NULL}.",
      "i" = "Current {.arg n_row} = {n_row}.",
      ">" = "Try a different {.arg n_row} value or inspect water potential values for outliers."
    ))
  }

  if (!psip_mod$convInfo$isConv) {
    cli::cli_abort(c(
      "Non-linear pressure potential model did not converge.",
      "i" = "Current {.arg n_row} = {n_row}.",
      ">" = "Try a different {.arg n_row} value or inspect your data."
    ))
  }

  psip <- as.vector(predict(psip_mod))

  srwc_tlp <- coef(psip_mod)[["r_tlp"]]
  pi_tlp <- pi_sat / srwc_tlp

  dataUpd <- x |>
    dplyr::mutate(
      invpsi = -1 / psi_vec,
      pio = pi_sat,
      psip_o = psip_sat,
      osmpot = pi_vec,
      prespot = psip,
      af = af,
      symrwc = srwc * 100,
      symrwd = srwd * 100,
      srwc_tlp = srwc_tlp * 100,
      pi_tlp = pi_tlp
    )

  structure(
    list(
      "psi" = psi_vec,
      "invpsi" = -1 / psi_vec,
      "pio" = pi_sat,
      "psip_o" = psip_sat,
      "osmpot" = pi_vec,
      "prespot" = psip,
      "af" = af,
      "symrwc" = srwc * 100,
      "symrwd" = srwd * 100,
      "srwc_tlp" = srwc_tlp * 100,
      "srwd_tlp" = 100 - srwc_tlp,
      "pi_tlp" = pi_tlp,
      "data" = dataUpd, # bring the data and model estimates along
      "est_rows" = n_row,
      "model" = pio$sma_mod
    ),
    units = c("MPa", "-MPa^-1", "MPa", "MPa", "MPa", "MPa", "%", "%", "%", "%", "%", "MPa"),
    class = "osmEst"
  )
}

#' @rdname estOsmotic
#' @param x An object of class "rwcEst"
#' @param n_row Number of rows above turgor loss
#' @param silent Silence printing of head(data) and columns used
#' @export
estOsmotic.rwcEst <- function(x, n_row = 5, silent = T, ...) {
  # validate input
  if (!inherits(x, "rwcEst")) {
    stop("`x` must be an object of class 'rwcEst'.")
  }

  # Pull the attached data frame; fail clearly if missing
  rwcEstData <- attr(x, "df")
  if (is.null(rwcEstData) || !is.data.frame(rwcEstData)) {
    stop("Attribute 'df' not found on `x` or is not a data.frame.")
  }

  # Print the data and what columns are being used.
  if (silent == FALSE) {
    cat("\nEstimating osmotic variables...\n\n")

    print(head(rwcEstData))

    nms <- names(x)

    cat(
      "Using the following columns for the estimation:\n",
      "{RWC/RWD}: ",
      nms[3],
      "\n",
      "{Water potential}: psi",
      "\n\n",
      sep = ""
    )
  }

  n <- nrow(rwcEstData)
  stopifnot(n_row >= 1L, n_row < n)

  # ---- Tail slices used for pio ----------------------------------------
  idx_tail <- (n - n_row + 1L):n # all values above and including tlp
  rwd_tail <- x$rwd[idx_tail]
  psi_tail <- rwcEstData$water.potential[idx_tail]

  # validated input rwd values
  if (any(is.na(psi_tail))) {
    stop("Tail water potentials contain NA.")
  }
  if (any(psi_tail == 0)) {
    stop("Tail water potentials contain zeros (cannot invert).")
  }

  minus_inv_psi <- -1 / psi_tail

  # pio estimate (osmotic potential at full turgor)
  pio <- estpio(rwd_tail, minus_inv_psi)
  pi_sat <- pio$pio # osmotic potential at full turgor 
  psip_sat <- -pi_sat

  # symplastic water content, osmotic, and pressure potential
  sym <- calc_symrwc(x$rwc, pio$sma_mod)
  af <- sym$af
  srwc <- sym$srwc
  srwd <- sym$srwd

  if (any(is.na(srwc) | srwc <= 0)) {
    stop("Saturated RWC must be positive.")
  }

  pi_vec <- pi_sat / srwc # osmotic potential
  psi_vec <- rwcEstData$water.potential # water potential
  psip_lin <- psi_vec - pi_vec # "linear" pressure potential
  #print(n_row)
  # fit sma to points above tlp to get initial guess at tlp
  fit_len <- n - n_row
  rtlp_fit <- sma_model(srwc[seq_len(fit_len)], psip_lin[seq_len(fit_len)])
  r_tlp_init <- -rtlp_fit$intercept / rtlp_fit$slope

  # fit nonlinear model
  psip_mod <- calc_nonlin_psip(
    data = data.frame(r = srwc, psip_linear = psip_lin),
    pi_sat = pi_sat,
    r_tlp = r_tlp_init
  )

  if (is.null(psip_mod)) {
    cli::cli_abort(c(
      "Non-linear pressure potential model returned {.val NULL}.",
      "i" = "Current {.arg n_row} = {n_row}.",
      ">" = "Try a different {.arg n_row} value or inspect water potential values for outliers."
    ))
  }

  if (!psip_mod$convInfo$isConv) {
    cli::cli_abort(c(
      "Non-linear pressure potential model did not converge.",
      "i" = "Current {.arg n_row} = {n_row}.",
      ">" = "Try a different {.arg n_row} value or inspect your data."
    ))
  }

  psip <- as.vector(predict(psip_mod))

  srwc_tlp <- coef(psip_mod)[["r_tlp"]]
  pi_tlp <- pi_sat / srwc_tlp

  dataUpd <- rwcEstData |>
    dplyr::mutate(
      rwcEstData,
      invpsi = -1 / psi_vec,
      pio = pi_sat,
      psip_o = psip_sat,
      osmpot = pi_vec,
      prespot = psip,
      af = af,
      symrwc = srwc * 100,
      symrwd = srwd * 100,
      srwc_tlp = srwc_tlp * 100,
      pi_tlp = pi_tlp
    )

  osm <- structure(
    list(
      "psi" = psi_vec,
      "invpsi" = -1 / psi_vec,
      "pio" = pi_sat,
      "psip_o" = psip_sat,
      "osmpot" = pi_vec,
      "prespot" = psip,
      "af" = af,
      "symrwc" = srwc * 100,
      "symrwd" = srwd * 100,
      "srwc_tlp" = srwc_tlp * 100,
      "srwd_tlp" = 100 - srwc_tlp,
      "pi_tlp" = pi_tlp,
      "data" = dataUpd, # bring the data and model estimates along
      "est_rows" = n_row,
      "model" = pio$sma_mod
    ),
    units = c("MPa", "-MPa^-1", "MPa", "MPa", "MPa", "MPa", "%", "%", "%", "%", "%", "MPa"),
    class = "osmEst"
  )

  return(osm)
}

# estOsmotic Helpers -----------------------------------------------------------------
#' Get varnames
#'
#' @param data Input data frame
#' @param wc.index Selected water content index
#' @param wp.index Selected water potential index
#'
#' @returns Variable names
get_varnames <- function(data, wc.index, wp.index) {
  nms <- names(data)

  if (is.character(wc.index) && is.character(wp.index)) {
    # verify columns exist
    miss <- setdiff(c(wc.index, wp.index), nms)
    if (length(miss)) {
      stop("Unknown column(s): ", paste(miss, collapse = ", "))
    }
    return(list(wc = wc.index, wp = wp.index))
  }

  if (is.numeric(wc.index) && is.numeric(wp.index)) {
    # numeric must be integerish and in bounds
    if (any(is.na(c(wc.index, wp.index)))) {
      stop("Indices cannot be NA.")
    }
    if (
      !all(wc.index == as.integer(wc.index)) ||
        !all(wp.index == as.integer(wp.index))
    ) {
      stop("Numeric indices must be integers.")
    }
    if (
      any(wc.index < 1 | wc.index > length(nms)) ||
        any(wp.index < 1 | wp.index > length(nms))
    ) {
      stop("Numeric indices out of bounds for `data`.")
    }
    return(list(wc = nms[wc.index], wp = nms[wp.index]))
  }

  stop("Both indices must be character (names) or both numeric (positions).")
}

#' Estimate symplastic water content
#'
#' @param rwc Vector of relative water content values.
#' @param sma_mod An object of class "sma_model" containing the slope and intercept of the linear relationship between relative water deficit and negative inverse water potential.
#' @return A vector of symplastic relative water content values.
#'
calc_symrwc <- \(rwc, sma_mod) {
  # apoplastic fraction
  # af is the x intercept of the relationship between rwd and negative inverse water potential to put it into terms of relative water content ADD 100
  af <- (sma_mod$intercept / sma_mod$slope) + 100

  # symplastic relative water content
  srwc_num <- rwc / 100 - af / 100
  srwc_den <- 1 - af / 100
  srwc <- srwc_num / srwc_den

  return(list(srwc = srwc, srwd = 100 - srwc, af = af))
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

  input_vals <- new_osminput(rwd, psi)

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

new_osminput <- function(rwd, psi) {
  inputs <- structure(
    list(rwd, psi),
    .Names = c("rwd", "neg_inv_psi"),
    class = "osm_input"
  )

  return(inputs)
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
  fit <- NULL
  start <- list(b = 1, r_tlp = r_tlp)
  low <- c(b = 0, r_tlp = start$r_tlp - 0.1)
  upper <- c(b = 2, r_tlp = start$r_tlp + 0.1)

  handle_nlsLM_error <- function(e) {
    msg <- conditionMessage(e)
    detail <- c(
      "x" = paste0("nlsLM error: ", msg),
      "i" = paste0(
        "Starting values: b = ", start$b,
        ", r_tlp = ", round(start$r_tlp, 4)
      ),
      "i" = paste0(
        "Bounds: b in [", low[["b"]], ", ", upper[["b"]], "], ",
        "r_tlp in [", round(low[["r_tlp"]], 4), ", ",
        round(upper[["r_tlp"]], 4), "]"
      ),
      "i" = paste0(
        "Data: ", nrow(data), " observations, ",
        "r range [", round(min(data$r), 4), ", ",
        round(max(data$r), 4), "]"
      )
    )
    if (grepl("singular gradient", msg, ignore.case = TRUE)) {
      detail <- c(detail,
        "!" = paste0(
          "A singular gradient usually means the initial r_tlp estimate ",
          "(", round(start$r_tlp, 4), ") is far from the true turgor loss ",
          "point, or the data lack sufficient curvature to identify the model."
        ),
        ">" = "Try adjusting `n_row` or check for outliers in water potential."
      )
    }
    cli::cli_warn(c(
      "Non-linear pressure potential model failed to fit.",
      detail
    ))
    return(NULL)
  }

  if (full == FALSE) {
    fit <- tryCatch(
      minpack.lm::nlsLM(
        psip_linear ~ -pi_sat * pmax((r - r_tlp) / (1 - r_tlp), 0)^b,
        data = data,
        start = start,
        lower = low,
        upper = upper,
        control = minpack.lm::nls.lm.control(maxiter = 1000)
      ),
      error = handle_nlsLM_error
    )
  } else {
    fit <- tryCatch(
      minpack.lm::nlsLM(
        psi_w ~
          (pi_sat / r) + -pi_sat * pmax((r - r_tlp) / (1 - r_tlp), 0)^b,
        data = data,
        start = start,
        lower = low,
        upper = upper,
        control = minpack.lm::nls.lm.control(maxiter = 1000)
      ),
      error = handle_nlsLM_error
    )
  }
  return(fit)
}


# Print methods -----------------------------------------------------------

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

#' Summary method of osmEst class objects
#' @param object Object of class osmEst
#' @param ... Other parameters passed to summary()
#' @export
summary.osmEst <- function(object, ...) {
  cat("Osmotic and pressure potential estimates: \n")
  cat("-----------------------------------------------\n")
  cat("Osmotic potential at full turgor:", round(object$pio, 2), "MPa", "\n")
  cat("Pressure potential at full turgor:", round(object$psip_o, 2), "MPa", "\n")
  cat("Apoplastic fraction:", round(object$af, 2), "%", "\n")
  cat("Number of hydration states used:", object$est_rows, "\n")
  cat("-----------------------------------------------\n")
}


