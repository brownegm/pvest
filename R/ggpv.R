#' Stat function for plotting pv curve on data

#' @param obj object of class estPV
#'
#' @return A list of patchwork plots (length = number of species) of water potential and water content relationships
#'
#' @importFrom ggplot2 ggplot geom_point geom_line aes geom_abline expand_limits theme_classic labs geom_vline geom_text annotate scale_color_manual
#' @importFrom patchwork plot_layout
#'
#' @export

plotPV <- function(obj) {
  pvbp <- lapply(seq_along(obj), \(x) attributes(obj[[x]])$breakpoint)

  splitpv <- pvest::split_all_dfs(obj, pvbp)
  # Fit models using your function
  fits <- lapply(
    splitpv,
    \(df) sma_model2(df, wp = "water.potential", lw = "fresh.weight")
  )
  fits_tlp <- lapply(splitpv, \(df) sma_model2(df, wp = "invpsi", lw = "rwd"))

  # Create and return the plot
  ## use a list to hold
  pvp_list <- vector(mode = "list", length = length(fits))

  for (i in seq_along(fits)) {
    lines_fw <- data.frame(
      label     = c("Above TLP", "Below TLP"),
      slope     = c(fits[[i]]$above$slope,     fits[[i]]$below$slope),
      intercept = c(fits[[i]]$above$intercept, fits[[i]]$below$intercept)
    )
    lines_tlp <- data.frame(
      label     = c("Above TLP", "Below TLP"),
      slope     = c(fits_tlp[[i]]$above$slope,     fits_tlp[[i]]$below$slope),
      intercept = c(fits_tlp[[i]]$above$intercept, fits_tlp[[i]]$below$intercept)
    )

    psiwater <- ggplot() +
      ggplot2::geom_point(
        data = obj[[i]],
        aes(x = .data[["fresh.weight"]], y = .data[["water.potential"]]),
        size = 4
      ) +
      ggplot2::geom_abline(
        data = lines_fw,
        aes(slope = slope, intercept = intercept, color = label),
        linewidth = 1.5
      ) +
      ggplot2::scale_color_manual(
        name = "",
        values = c("Above TLP" = "forestgreen", "Below TLP" = "black")
      ) +
      ggplot2::expand_limits(y = -0.1) +
      ggplot2::theme_classic(base_size = 18) +
      ggplot2::labs(
        y = expression(paste(Psi["leaf"], " (MPa)")),
        x = "Water Mass (g)",
        title = "Plateau effect, Saturated Water Content"
      )

    invpsi_rwc <- ggplot() +
      ggplot2::geom_point(
        data = obj[[i]],
        aes(x = .data[["rwd"]], y = .data[["invpsi"]]),
        size = 4
      ) +
      ggplot2::geom_abline(
        data = lines_tlp,
        aes(slope = slope, intercept = intercept, color = label),
        linewidth = 1.5
      ) +
      ggplot2::scale_color_manual(
        name = "",
        values = c("Above TLP" = "forestgreen", "Below TLP" = "black")
      ) +
      ggplot2::annotate(
        "text",
        x = max(obj[[i]]$rwd, na.rm = TRUE) - 10,
        y = max(obj[[i]]$invpsi, na.rm = TRUE),
        label = bquote(pi[TLP] == .(round(unique(obj[[i]]$pi_tlp), 2)) ~ MPa),
        hjust = 0,
        vjust = 1,
        size = 5
      ) +
      ggplot2::theme_classic(base_size = 18) +
      ggplot2::labs(
        y = expression(frac(-1, paste(Psi["leaf"])), "(MPa)"),
        x = "Relative Water Deficit (%)",
        title = "Turgor Loss Point"
      )

    psiprwc <- ggplot() +
      ggplot2::geom_point(
        data = obj[[i]],
        aes(x = .data[["symrwd"]], y = .data[["prespot"]]),
        size = 4
      ) +
      ggplot2::annotate(
        "text",
        x = min(obj[[i]]$symrwd, na.rm = TRUE),
        y = max(obj[[i]]$prespot, na.rm = TRUE),
        label = paste("RWCtlp =", unique(obj[[i]]$srwc_tlp) |> round(2), "%"),
        hjust = 0,
        vjust = 1,
        size = 5
      ) +
      ggplot2::geom_vline(
        xintercept = 100 - unique(obj[[i]]$srwc_tlp),
        linewidth = 2,
        color = "forestgreen"
      ) +
      ggplot2::theme_classic(base_size = 18) +
      ggplot2::labs(
        y = expression(paste(Psi["p"], "(MPa)")),
        x = "Symplastic Relative Water Deficit (%)",
        title = "Inflection Detection"
      )

    pvplot <- psiwater / invpsi_rwc / psiprwc

    pvp_list[[i]] <- pvplot
  }

  return(pvp_list)
}

# #' Draw line segments on plots
# #'
# #' @param fit A fit object from a object of class PVest
# #' @param x_range Range of independent variable
# #' @param color Line color
# #' @param ... Additional parameters passed to geom_segment
# #'
# #' @return Segments for the segmented regression
# #' @export
#
# drawSegments <- function(fit, x_range, color = "black", ...) {
#   data.frame(
#     x = x_range[1],
#     xend = x_range[2],
#     y = fit$slope * x_range[1] + fit$intercept,
#     yend = fit$slope * x_range[2] + fit$intercept
#   ) %>%
#     ggplot2::geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = color, ...)
# }
