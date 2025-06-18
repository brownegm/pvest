#' Stat function for plotting pv curve on data

#' @param obj object of class estPV
#' @param ... additional arguments not used
#' 
#' @return A list of patchwork plots (length = number of species) of water potential and water content relationships
#' 
#' @importFrom ggplot2 ggplot ggproto Stat geom_point geom_line aes
#' @importFrom patchwork plot_layout
#' @importFrom purrr map
#' 
#' @export

## Create ggproto object

plotPV <- function(obj, ...) {
  
  # get breakpoints
  pvbp <- purrr::map(obj, ~ attributes(.x)$breakpoint)

  # split dfs into above and below 
  splitpv <- pvest:::split_all_dfs(obj, pvbp)

  # Fit models using your function
  fits <- purrr::map(splitpv, ~ sma_model2(.x, wp = "water.potential", lw = "fresh.weight"))
  fits_tlp <- purrr::map(splitpv, ~ sma_model2(.x, wp = "invpsi", lw = "rwd"))

  # Create and return the plots
  pvp_list <- imap(fits, \(f) {

    pvd <- obj[[f]]
    fit_tlp <- fits_tlp[[f]]
    bp <- pvbp[[f]]

  # Define x-ranges for segments
  x_fw <- range(pvd$fresh.weight, na.rm = TRUE)
  x_rwd <- range(pvd$rwd, na.rm = TRUE)
  
   # Below and above breakpoint ranges for fresh weight
    x_below <- c(x_fw[1], bp)
    x_above <- c(bp, x_fw[2])
    
    psiwater <- ggplot2::ggplot(data = pvd, aes(x = fresh.weight, y = water.potential)) +
      ggplot2::geom_point(size = 4) +

      # draw lines
      drawSegment(pvd$below, x_below, color = "black", linewidth = 1.5)+
      drawSegment(pvd$above, x_above, color = "forestgreen", linewidth = 1.5)+

      # aesthetics
      ggplot2::expand_limits(y = -0.1) +
      ggplot2::theme_classic(base_size = 18) +
      ggplot2::labs(
        y = expression(paste(Psi["leaf"], " (MPa)")),
        x = "Water Mass (g)",
        title = "Plateau effect, Saturated Water Content"
      )

    invpsi_rwc <- ggplot() +
      ggplot2::geom_point(data = pvd, aes(x = rwd, y = invpsi), size = 4) +
      # draw linese
      drawSegment(fit_tlp$below, x_rwd, color = "black", linewidth = 1.5) +
      # aesthetics
      ggplot2::theme_classic(base_size = 18) +
      ggplot2::labs(
        y = expression(frac(-1, paste(Psi["leaf"])), "(MPa)"),
        x = "Relative Water Deficit (%)",
        title = "Turgor Loss Point"
      )

   psiwater / invpsi_rwc
  })
  
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

drawSegment <- function(fit, x_range, color = "black", ...) {
  xvals <- fit$data[[1]]
  yvals <- fit$fitted

  data.frame(
    x = fit$data[[1]],
    xend = x_range[2],
    y = fit$fitted,
    yend = sk
  ) %>%
    ggplot2::geom_segment(aes(x = xvals , y = yvals, xend = , yend = yend), color = color, ...)
}
