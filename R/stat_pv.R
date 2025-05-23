# #' Compute line segments from slope, intercept, and x-range
# #'
# #' @description `stat_segment_fit()` is a ggplot2 stat that draws line segments
# #' defined by slope, intercept, and x-range. It’s useful for plotting clipped
# #' linear fits without creating models or formulas.
# #'
# #' @param mapping Set of aesthetic mappings.
# #' @param data Data frame with slope, intercept, xmin, and xmax columns.
# #' @param geom Geom to use (defaults to "segment").
# #' @param position Position adjustment.
# #' @param na.rm Logical. Remove missing values?
# #' @param show.legend Whether to include in legend.
# #' @param inherit.aes Inherit default ggplot aesthetics?
# #' @param ... Other arguments passed to layer.
# #'
# #' @return A ggplot2 layer that draws line segments from linear fits.
# #' @export
# #' @import ggplot2
# stat_pv <- function(mapping = NULL, data = NULL, geom = "segment",
#                              position = "identity", na.rm = FALSE, show.legend = NA,
#                              inherit.aes = FALSE, ...) {
#   ggplot2::layer(
#     stat = StatPV,
#     mapping = mapping,
#     data = data,
#     geom = geom,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, ...)
#   )
# }

# #' ggproto for stat_segment_fit
# #'
# #' @keywords internal
# #' @export
# StatPV <- ggplot2::ggproto(
#   "StatPV",
#   ggplot2::Stat,
#   required_aes = c("slope", "intercept", "xmin", "xmax"),
  
#   compute_group = function(data, scales) {
#     transform(
#       data,
#       x = xmin,
#       xend = xmax,
#       y = intercept + slope * xmin,
#       yend = intercept + slope * xmax
#     )
#   }
# )


# fit_df <- data.frame(
#   slope = c(fits[[1]]$below$slope, fits[[1]]$above$slope),
#   intercept = c(fits[[1]]$below$intercept, fits[[1]]$above$intercept),
#   xmin = c(min_below, min_above),
#   xmax = c(max_below, max_above),
#   group = c("below", "above"),
#   color = c("black", "forestgreen")
# )

# ggplot(data = obj[[1]], aes(x = fresh.weight, y = water.potential)) +
#   geom_point(size = 4) +
#   stat_segment_fit(data = fit_df,
#                    aes(slope = slope, intercept = intercept,
#                        xmin = xmin, xmax = xmax, color = group),
#                    linewidth = 1.5) +
#   scale_color_manual(values = c("below" = "black", "above" = "forestgreen")) +
#   theme_classic()
