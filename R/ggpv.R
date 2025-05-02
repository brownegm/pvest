#' Stat function for plotting pv curve on data

#'@importFrom ggplot2 ggplot ggproto Stat
#'@import patchwork
#@inheritParams ggplot2::stat_smooth
#'@export

## Create ggproto object

plotPV <- function(obj, ...) {
  
  pvbp <- lapply(seq_along(obj), \(x) attributes(obj[[x]])$breakpoint)
  
  splitpv <- pvest:::split_all_dfs(obj, pvbp)
  # Fit models using your function
  fits <- lapply(splitpv, \(df) sma_model2(df, wp = "water.potential", lw = "fresh.weight"))
  
  # Create and return the plot
  psiwater <- ggplot() +
    geom_point(data = obj[[1]],
               aes(x = fresh.weight, y = water.potential),
               size = 3) +
    geom_abline(
      slope = fits[[1]]$above$slope,
      intercept = fits[[1]]$above$intercept,
      color = "red"
    ) +
    geom_abline(
      slope = fits[[1]]$below$slope,
      intercept = fits[[1]]$below$intercept,
      color = "blue"
    ) +
    theme_classic(base_size = 16) +
    labs(y = "Water Potential", x = "Leaf water")
  
  invpsi_rwc <- ggplot() +
    geom_point(data = obj[[1]],
               aes(x = rwd, y = -1 / water.potential),
               size = 3) +
    geom_abline(
      slope = fits[[1]]$below$slope,
      intercept = fits[[1]]$below$intercept,
      color = "blue"
    ) +
    theme_classic(base_size = 16) +
    labs(y = "-1/Water Potential", x = "Relative Water Deficit (%)")+
    geom_smooth(data = obj[[1]], aes(x = rwc, y = -1/water.potential ))
  
  pvplot <- psiwater/invpsi_rwc 
}
#' 
#' StatPV <- ggplot2::ggproto(
#'   "StatPV",
#'   ggplot2::Stat,
#'   required_aes = c("x", "y"),
#'   
#'   compute_group = function(data, scales, model_fun, attr_name =
#'                              "modele", ...) {
#'     # Extract model elements from model_fun
#'     model_obj <- estPV(data, ...)
#'     
#'     # Get the line parameters from the attribute
#'     line_params <- attr(model_obj, attr_name)
#'     
#'     line_params$breakpoint <- 
#'     if (is.null(line_params)) {
#'       stop(paste0(
#'         "The attribute '",
#'         attr_name,
#'         "' was not found in the model object"
#'       ))
#'     }
#'     
#'     
#'     ggplot2::ggplot(data = aes(x = ))
#'     # Generate points for the two line segments
#'     x_range <- range(data$x)
#'     
#'     # Create sequence of x values for each segment
#'     x_before <- seq(x_range[1], breakpoint, length.out = 100)
#'     x_after <- seq(breakpoint, x_range[2], length.out = 100)
#'     
#'     # Calculate y values using the slope and intercept
#'     y_before <- segment1$intercept + segment1$slope * x_before
#'     y_after <- segment2$intercept + segment2$slope * x_after
#'     
#'     # Combine the data for plotting
#'     result <- data.frame(
#'       x = c(x_before, x_after),
#'       y = c(y_before, y_after),
#'       segment = factor(c(
#'         rep("before", length(x_before)), rep("after", length(x_after))
#'       ))
#'     )
#'     
#'     return(result)
#'   }
#' )
#' 
#' #' Plot a two-part line based on extracted model elements
#' #'
#' #' This stat extracts slope and intercept values from attributes of a model object
#' #' and creates a visualization of a two-part line.
#' #'
#' #' @param mapping Set of aesthetic mappings
#' #' @param data The data to be displayed in this layer
#' #' @param geom The geometric object to use display the data
#' #' @param position Position adjustment
#' #' @param model_fun Function that returns model with slope/intercept attributes
#' #' @param attr_name The name of the attribute containing slope/intercept values
#' #' @param ... Other arguments passed on to \code{layer()}
#' #' @param na.rm If \code{FALSE}, the default, missing values are removed with
#' #'   a warning. If \code{TRUE}, missing values are silently removed.
#' #' @param show.legend logical. Should this layer be included in the legends?
#' #' @param inherit.aes If \code{FALSE}, overrides the default aesthetics
#' #'
#' #' @import ggplot2
#' #' @export
#' stat_two_part_line <- function(mapping = NULL,
#'                                data = NULL,
#'                                geom = "path",
#'                                position = "identity",
#'                                model_fun = NULL,
#'                                attr_name = "line_params",
#'                                ...,
#'                                na.rm = FALSE,
#'                                show.legend = NA,
#'                                inherit.aes = TRUE) {
#'   # Validate input
#'   if (is.null(model_fun)) {
#'     stop("You must provide a model function via 'model_fun'")
#'   }
#'   
#'   ggplot2::layer(
#'     data = data,
#'     mapping = mapping,
#'     stat = StatTwoPartLine,
#'     geom = geom,
#'     position = position,
#'     show.legend = show.legend,
#'     inherit.aes = inherit.aes,
#'     params = list(
#'       model_fun = model_fun,
#'       attr_name = attr_name,
#'       na.rm = na.rm,
#'       ...
#'     )
#'   )
#' }
#' 
#' # Define the ggproto object
#' 
#' # Example usage:
#' #
#' # # Define a model function that returns an object with line parameters as an attribute
#' # my_model_fun <- function(data, ...) {
#' #   # Perform your modeling
#' #   model_result <- some_calculation(data)
#' #
#' #   # Add line parameters as an attribute
#' #   attr(model_result, "line_params") <- list(
#' #     breakpoint = 5.0,
#' #     segment1 = list(intercept = 1.2, slope = 0.8),
#' #     segment2 = list(intercept = 3.5, slope = 0.3)
#' #   )
#' #
#' #   return(model_result)
#' # }
#' #
#' # # Plot using the stat
#' # ggplot(data, aes(x = x, y = y)) +
#' #   stat_two_part_line(model_fun = my_model_fun) +
#' #   geom_point()