#' Estimate the RWC and RWD at turgor loss point for using the relationship between leaf pressure potential and
#'     bulk and symplastic relative water deficit
#'
#' @param data A data frame. Must contain pressure potential and relative water deficit values.
#' @param psip.index A string or number.An indexing value for the vector in data for pressure potential.
#' @param rwd.index A string or number.An indexing value for the vector in data for relative water deficit.
#' @param sym_rwd.index A string or number.An indexing value for the vector in data for symplastic relative water deficit.
#'
#' @return Bulk and symplastic slopes and intercepts.
#' @export
#'
psip_rwd_slopeint <- function(psip, rwd, sym_rwd) {

  #set input class for correct input type 
  inputs <- structure(list(psip, rwd, sym_rwd),.Names =  c( "psip", "rwd", "sym_rwd"), class="osm_input")

  bulk <- sma_model(psip,rwd)
  symplastic <- sma_model(psip,sym_rwd)

  # slope <- -sma_slope(x = psip, y = data[, rwd.index])
  # intercept <- sma_intercept(x = psip, y = data[, rwd.index], slope = slope)

  # slope_sym <- -sma_slope(x = psip, y = data[, sym_rwd.index])
  # intercept_sym <- sma_intercept(x = psip, y = data[, sym_rwd.index], slope = slope_sym)

  out <- list("bulk_slope" = bulk$slope,
              "bulk_int" = bulk$intercept,
              "sym_slope" = symplastic$slope,
              "sym_int" = symplastic$intercept)
  
  # out <- list("bulk_slope" = slope,
  #             "bulk_int" = intercept,
  #             "sym_slope" = slope_sym,
  #             "sym_int" = intercept_sym)
  invisible(out)
}
#' Estimate parameters at turgor loss
#' @description Estimate leaf water potential, relative water content at turgor loss point and modulus of elasticity
#'    both bulk parameters and their symplastic counterparts
#'
#' @param data A data frame
#' @param wc.index An unquoted string indicating vector with water associated variable
#' @param wp.index An unquoted string indicating vector with pressure associated variable
#' @param n_row_above Double; Default to 4 rows.Value which indicates the number of rows above TLP for estimating SMA line parameters.
#' @param n_row_below Double; Default to 4 rows.Value which indicates the number of rows below TLP for estimating SMA line parameters.
#'
#' @return Returns data with new columns for the estimated parameters.
#'
#' @import dplyr
#' @export

estTLP <- function(x,...){
  UseMethod("estTLP")
}


#' Default method for estTLP
#' @export
estTLP.default <- function(data, wc.index, wp.index, n_row_above = 4, n_row_below = 4) {
  
  data_belowtlp <- df %>%
    dplyr::arrange(desc({{ wp.index }})) %>%
    dplyr::slice_tail(n = n_row_below) %>%
    as.data.frame()

  data_abovetlp <- df %>%
    dplyr::arrange(desc({{ wp.index }})) %>%
    dplyr::slice_head(n = n_row_above) %>%
    as.data.frame()

  psip_rwd_list <- psip_rwd_slopeint(
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
  class= "tlp")
  return(outtlp)
}

NULL

#' estTLP method 
#' @description A method of estTLP for use with objects that are of class "osmEst"(i.e., returned from pvest::estOsm())
#' @export
estTLP.osmEst <- function(osm_object, wc.index, wp.index, n_row_above = 4, n_row_below = 4) {
  
  # for an object of class osmEst
  data_abovetlp <- osm_object$data %>%
    dplyr::arrange(desc({{ wp.index }})) %>%
    dplyr::slice_head(n = n_row_above) %>%
    as.data.frame()

  psip.rwd_list <- psip_rwd_slopeint(
    data = data_abovetlp,
    psip.index = "pressure.potential",
    rwd.index = "relative.water.deficit",
    sym_rwd.index = "sym.rwd"
  ) # output is a list in order: slope, intercept, sym slope, sym intercept

  osm_slope <- osm_object$model$slope
  osm_intercept <- osm_object$model$intercept

  outtlp <- structure(list(
    "rwd_tlp" <- -((psip.rwd_list[2]) / (psip.rwd_list[1])),
    "rwc_tlp"<- 100 - relative.water.deficit.attlp,
    "sym_rwd_tlp" <- -((psip.rwd_list[4]) / (psip.rwd_list[3])),
    "sym_rwc_tlp" <- 100 - sym.rwd.attlp,
    "pi_tlp"<- -1 / (osm_slope * relative.water.deficit.attlp + osm_intercept),
    "modulus" <- max.psip / ((100 - relative.water.content.attlp) / 100),
    "modulus_sym" <- max.psip / ((100 - sym.rwc.attlp) / 100)
    ), 
    class= "tlp")
  
  return(outtlp)
}