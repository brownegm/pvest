#' Estimate the RWC and RWD at turgor loss point for using the relationship between leaf pressure potential and bulk and symplastic relative water deficit
#'
#' @param psip A vector of pressure potential values
#' @param rwd A vector of relative water deficit values
#' @param sym_rwd A vector of symplastic relative water deficit values
#'
#' @return Bulk and symplastic slopes and intercepts.

psip_rwd_params <- function(psip, rwd, sym_rwd) {
  # create an input class for the data
  tlp_input <- tlp_input(psip, rwd, sym_rwd)
  # construct bulk and symplastic models
  tlp_model <- sma_model(tlp_input)

  invisible(tlp_model)
}

#' Constructor for the tlp_input class
tlp_input <- function(psip, rwd, sym_rwd) {
  input <- structure(list(psip, rwd, sym_rwd),
                     .Names=c("psip", "rwc", "sym_rwc"),
            class = "tlp_input")
  return(input)
}


#' Estimate parameters at turgor loss
#' @description Estimate leaf water potential, relative water content at turgor loss point and modulus of elasticity both bulk parameters and their symplastic counterparts
#'
#' @param data A data frame
#' @param wc.index An unquoted string indicating vector with water associated variable
#' @param wp.index An unquoted string indicating vector with pressure associated variable
#' @param n_row_above Double; Default to 4 rows.Value which indicates the number of rows above TLP for estimating SMA line parameters.
#' @param n_row_below Double; Default to 4 rows.Value which indicates the number of rows below TLP for estimating SMA line parameters.
#'
#' @return Returns data with new columns for the estimated parameters.
#'
#' @importFrom dplyr arrange slice_tail slice_head
#' @export
#' @rdname estTLP

estTLP <- function(x,...){
  UseMethod("estTLP")
}

#' @export
#' @rdname estTLP
estTLP.default <- function(data, wc.index, wp.index, n_row_above = 4, n_row_below = 4) {
  
  data_belowtlp <- df %>%
    dplyr::arrange(desc({{ wp.index }})) %>%
    dplyr::slice_tail(n = n_row_below) %>%
    as.data.frame()

  data_abovetlp <- df %>%
    dplyr::arrange(desc({{ wp.index }})) %>%
    dplyr::slice_head(n = n_row_above) %>%
    as.data.frame()

  psip_rwd_list <- psip_rwd_params(
    data = data_abovetlp,
    psip.index = "pressure.potential",
    rwd.index = "relative.water.deficit",
    sym_rwd.index = "sym.rwd"
  ) # output is a list in order: slope, intercept, sym slope, sym intercept

  pi.o_list <- estOsmotic(data = data_belowtlp, wc.index, wp.index)

  outtlp <- structure(list(
  "rwd_tlp" <- -((psip.rwd_list[2]) / (psip.rwd_list[1])),
  "rwc_tlp"<- 100 - relative.water.deficit.attlp,
  "sym_rwd_tlp" <- -((psip.rwd_list[4]) / (psip.rwd_list[3])),
  "sym_rwc_tlp" <- 100 - sym.rwd.attlp,
  "pi_tlp"<- -1 / (pi.o_list[1] * relative.water.deficit.attlp + pi.o_list[2]),
  "modulus" <- max.psip / ((100 - relative.water.content.attlp) / 100),
  "modulus_sym" <- max.psip / ((100 - sym.rwc.attlp) / 100)
  ), 
  class= "tlpEst")
  return(outtlp)
}

NULL


#' @export
#' @rdname estTLP
estTLP.osmEst <- function(osm_obj, n_row_above = 4) {
  
  above_idx <- c(1:n_row_above)
  
  param_list <- psip_rwd_slopeint(
    psip = osm_obj$prespot[above_idx],
    rwd = osm_obj$rwd[above_idx], 
    sym_rwd = osm_obj$sym_rwd[above_idx]
  ) 
  
  #collect model parameters for below from osm object
  osm_slope <- osm_object$model$slope
  osm_intercept <- osm_object$model$intercept

  outtlp <- structure(list(
    "rwd_tlp" = -((param_list$intercept) / (param_list$slope)),
    "rwc_tlp"= 100 - relative.water.deficit.attlp,
    "sym_rwd_tlp" = -((param_list$intercept_sym) / (param_list$slope_sym)),
    "sym_rwc_tlp" = 100 - sym.rwd.attlp,
    "pi_tlp" = -1 / (osm_slope * relative.water.deficit.attlp + osm_intercept),
    "modulus" = max.psip / ((100 - relative.water.content.attlp) / 100),
    "modulus_sym" = max.psip / ((100 - sym.rwc.attlp) / 100)
    ), 
    class= "tlpEst")
  
  return(outtlp)
}