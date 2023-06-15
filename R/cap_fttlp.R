#' Estimate the capacitance at full turgor and turgor loss point for the symplast and total.
#'
#' @param df A data frame
#' @param wp.index A string. Indicates water potential vectror in df. Default is "water.potential".
#' @param wc.index Water content index. Here the expected value is total relative water content. Default is "relative.water.content".
#' @param s_wc.index Symplastic water content index. Here symplastic relative water content is expected. Default is "sym.rwc".
#'
#' @return Returns vectors of estimated capacitance above and below turgor loss point
#'     and for bulk tissue values and symplastic quantities.
#'
#' @import dplyr
#' @importFrom rlang sym
#' @export

capacitance_fttlp <- function(df, wp.index = "water.potential", wc.index = "relative.water.content", s_wc.index = "sym.rwc") {
  wp.sym <- rlang::sym(wp.index) # create symbol from string input for later tidy eval

  psi.tlp <- unique(df$leaf.waterpotential.attlp) # unique value of psi tlp for leaf
  stopifnot("Psi_tlp must be numeric" = is.numeric(psi.tlp)) # make sure tlp is numeric

  # extract values above and below turgor loss point
  data_abovetlp <- df %>%
    dplyr::arrange(desc({{ wp.sym }})) %>%
    dplyr::filter({{ wp.sym }} > psi.tlp) %>%
    as.data.frame()

  data_belowtlp <- df %>%
    dplyr::arrange(desc({{ wp.sym }})) %>%
    dplyr::filter({{ wp.sym }} < psi.tlp) %>%
    as.data.frame()

  # estimate capacitance
  cap.ft.bulk <- (sma_slope(x = data_abovetlp[, wc.index], y = data_abovetlp[, wp.index])) / 100
  cap.ft.sym <- (sma_slope(x = data_abovetlp[, s_wc.index], y = data_abovetlp[, wp.index])) / 100
  cap.tlp.bulk <- (sma_slope(x = data_belowtlp[, wc.index], y = data_belowtlp[, wp.index])) / 100
  cap.tlp.sym <- (sma_slope(x = data_belowtlp[, s_wc.index], y = data_belowtlp[, wp.index])) / 100

  output <- c(cap.ft.bulk, cap.ft.sym, cap.tlp.bulk, cap.tlp.sym)

  return(output)
}
