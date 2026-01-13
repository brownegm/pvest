#' @importFrom ggplot2 layer geom_segment
#' @export
geom_clipped_fit <- function(slope, intercept, xrange, color = "black", linewidth = 1.2) {
  data <- data.frame(
    x = xrange[1],
    xend = xrange[2],
    y = intercept + slope * xrange[1],
    yend = intercept + slope * xrange[2]
  )
  geom_segment(data = data,
               aes(x = x, xend = xend, y = y, yend = yend),
               color = color, linewidth = linewidth,
               inherit.aes = FALSE)
}

ggplot() +
  geom_point(...) +
  geom_clipped_fit(fits[[1]]$above$slope, fits[[1]]$above$intercept, above_range, color = "forestgreen") +
  geom_clipped_fit(fits[[1]]$below$slope, fits[[1]]$below$intercept, below_range, color = "black")


# autoplot ----------------------------------------------------------------



# Constructor
as.clipped_lm_fit <- function(fit, xrange, ...) {
  structure(list(
    slope = fit$slope,
    intercept = fit$intercept,
    xrange = xrange,
    ...
  ), class = "clipped_lm_fit")
}

#' @importFrom ggplot2 autoplot geom_segment aes
#' @export
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
    color = object$color %||% "black",
    linewidth = object$linewidth %||% 1.2
  )
}

autoplot(as.clipped_lm_fit(fits[[1]]$above, range_above))
