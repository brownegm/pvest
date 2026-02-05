#' Stat function for plotting pv curve on data

#' @param obj object of class estPV
#' @param ... additional arguments not used
#' 
#' @return A list of patchwork plots (length = number of species) of water potential and water content relationships
#' 
#' @importFrom ggplot2 ggplot ggproto Stat geom_point geom_line aes
#' @importFrom patchwork plot_layout
#' 
#' @export

plotPV <- function(obj, ...) {
  pvbp <- lapply(seq_along(obj), \(x) attributes(obj[[x]])$breakpoint)

  splitpv <- pvest:::split_all_dfs(obj, pvbp)
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
    psiwater <- ggplot() +
      ggplot2::geom_point(
        data = obj[[i]],
        aes(x = fresh.weight, y = water.potential),
        size = 4
      ) +
      ggplot2::geom_abline(
        slope = fits[[i]]$above$slope,
        intercept = fits[[i]]$above$intercept,
        color = "forestgreen",
        linewidth = 1.5
      ) +
      ggplot2::geom_abline(
        slope = fits[[i]]$below$slope,
        intercept = fits[[i]]$below$intercept,
        color = "black",
        linewidth = 1.5
      ) +
      ggplot2::expand_limits(y = -0.1) +
      ggplot2::theme_classic(base_size = 18) +
      ggplot2::labs(
        y = expression(paste(Psi["leaf"], " (MPa)")),
        x = "Water Mass (g)",
        title = "Plateau effect, Saturated Water Content"
      )

    invpsi_rwc <- ggplot() +
      ggplot2::geom_point(data = obj[[i]], aes(x = rwd, y = invpsi), size = 4) +
      ggplot2::geom_abline(
        slope = fits_tlp[[i]]$below$slope,
        intercept = fits_tlp[[i]]$below$intercept,
        color = "black",
        linewidth = 1.5
      ) +
      ggplot2::theme_classic(base_size = 18) +
      ggplot2::labs(
        y = expression(frac(-1, paste(Psi["leaf"])), "(MPa)"),
        x = "Relative Water Deficit (%)",
        title = "Turgor Loss Point"
      )
    
    result_summary <- tibble(
      x = 20, 
      y = 1,
      text = paste("RWCtlp = ", unique(obj[[i]]$srwc_tlp) |> round(2), "%")
    )
    
    psiprwc <- ggplot() +
      ggplot2::geom_point(data = obj[[i]], aes(x = symrwd, y = prespot), size = 4)+
      ggplot2::geom_text(data = result_summary, 
                         aes(x, y, label = text), inherit.aes = FALSE)+
      ggplot2::geom_vline(xintercept = 100-unique(obj[[i]]$srwc_tlp, linewidth = 2),
                          color = "forestgreen") +
      ggplot2::theme_classic(base_size = 18) +
      ggplot2::labs(
        y = expression(paste(Psi["p"], "(MPa)")),
        x = "Symplastic Relative Water Deficit (%)",
        title = "Inflection Detection"
      )

    pvplot <- psiwater / invpsi_rwc / psiprwc

    pvp_list[[i]] <- pvplot
  }
  #return(pvplot)
  return(pvp_list)
}


#' Draw line segments on plots
#' 
#' @param fit A fit object from a object of class PVest
#' @param x_range Range of independent variable
#' @param color Line color
#' @param ... Additional parameters passed to geom_segment
#' 
#' @return Segments for the segmented regression 
#' @export 

drawSegments <- function(fit, x_range, color = "black", ...) {
  data.frame(
    x = x_range[1],
    xend = x_range[2],
    y = fit$slope * x_range[1] + fit$intercept,
    yend = fit$slope * x_range[2] + fit$intercept
  ) %>%
    ggplot2::geom_segment(aes(x = x, y = y, xend = xend, yend = yend), color = color, ...)
}
