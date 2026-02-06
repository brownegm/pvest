#' Create a clipped linear fit object for plotting
#'
#' @param fit A list with elements `slope` and `intercept`.
#' @param xrange Numeric vector of length 2 giving the x-range over which to plot the line.
#' @param color Line color (default "black").
#' @param linewidth Line width (default 1.2).
#'
#' @return An object of class "clipped_lm_fit".
#' @export
as.clipped_lm_fit <- function(fit, xrange, color = "black", linewidth = 1.2) {
  structure(list(
    slope = fit$slope,
    intercept = fit$intercept,
    xrange = xrange,
    color = color,
    linewidth = linewidth
  ), class = "clipped_lm_fit")
}

#' Autoplot method for clipped linear fit
#'
#' @param object A `clipped_lm_fit` object created by [as.clipped_lm_fit()].
#' @param ... Additional arguments (unused).
#'
#' @return A `ggplot2` layer (a clipped `geom_segment`).
#' @export
#' @importFrom ggplot2 geom_segment aes
autoplot.clipped_lm_fit <- function(object, ...) {
  df <- data.frame(
    x = object$xrange[1],
    xend = object$xrange[2],
    y = object$intercept + object$slope * object$xrange[1],
    yend = object$intercept + object$slope * object$xrange[2]
  )
  ggplot2::geom_segment(
    data = df,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    color = object$color,
    linewidth = object$linewidth
  )
}

#' Plot a clipped linear fit directly
#'
#' @param slope Slope of the line.
#' @param intercept Intercept of the line.
#' @param xrange Numeric vector of length 2 giving the x-range.
#' @param color Line color.
#' @param linewidth Line width.
#'
#' @return A `geom_segment` layer clipped to the given x-range.
#' @export
#' @importFrom ggplot2 geom_segment aes
geom_clipped_fit <- function(slope, intercept, xrange, color = "black", linewidth = 1.2) {
  df <- data.frame(
    x = xrange[1],
    xend = xrange[2],
    y = intercept + slope * xrange[1],
    yend = intercept + slope * xrange[2]
  )
  ggplot2::geom_segment(
    data = df,
    aes(x = x, xend = xend, y = y, yend = yend),
    color = color,
    linewidth = linewidth,
    inherit.aes = FALSE
  )
}
