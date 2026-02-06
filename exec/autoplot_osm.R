# Plotting ----------------------------------------------------------------

#' Plot method of class osmEst
#'
#' @param object Object of class osmEst
#' @param xlab x label
#' @param ylab y label
#' @param main Title main
#' @param lwd line width
#' @param lty line type
#' @param ... unused
#'
#' @returns Hofler diagram of osmotic elements
#' @export
#' @importFrom ggplot2 ggplot theme_classic
autoplot.osmEst <- function(
    object,
    xlab = "100-RWC",
    ylab = expression(Psi),
    main = "Hofler Diagram",
    lwd = 1.5,
    lty = 1,
    ...
) {
  if (!inherits(object, "osmEst")) {
    stop("The object must be of class 'osmEst'.")
  }
  # legend names
  legendNames <- c(
    expression(Psi["s"]),
    expression(Psi["p"]),
    expression(Psi["leaf"])
  )
  
  pData <- object$data |>
    tidyr::pivot_longer(
      c(psi, osmpot:prespot),
      names_to = "variable",
      values_to = "mpa"
    )
  
  #tlp <- data.frame(r_tlp = object$srwc_tlp, pi_tlp = object$pi_tlp)
  
  pv <- ggplot2::ggplot(pData, ggplot2::aes(x = rwd, y = mpa, color = variable)) +
    ggplot2::geom_line(lwd = lwd, lty = lty) +
    #geom_point(data = tlp, aes(x =100-r_tlp, y = pi_tlp), color = "brown", shape = 8)+
    ggplot2::theme_classic() +
    ggplot2::labs(x = xlab, y = ylab, color = "Variable", title = main) +
    ggplot2::scale_color_discrete(labels = legendNames) +
    ggplot2::theme_classic()
  
  return(pv)
}
