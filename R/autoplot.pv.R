#' ## autoplot method replicating hofler diagram
#' 
#' #' Plotting method for pv objects
#' #'
#' #' @param object A pv object
#' #' @param xlab Label for x-axis
#' #' @param ylab Label for y-axis
#' #' @param main Main title for the plot
#' #' @param lwd Line width
#' #' @param lty Line type
#' #' @param ... Additional arguments passed to autoplot generic
#' #'
#' #' @returns A ggplot object representing the Hofler diagram
#' #' 
#' #' @import ggplot2
#' #' @importFrom tidyr pivot_longer
#' #' @importFrom woodcut theme_woodcut
#' 
#' autoplot.pv <- function(object, xlab = "100-RWC", ylab = expression(Psi), 
#'                         main = "Hofler Diagram", lwd = 1.5, lty = 1, 
#'                         ...) {
#'   
#'   require(aes)
#'   # if (!inherits(object, "pv")) {
#'   #   stop("The object must be of class 'pv'.")
#'   # }
#'   # legend names 
#'   legendNames <- c(expression(Psi["p,linear"]),
#'                    expression(Psi["p,nonlinear"]),
#'                    expression(Psi["s"]),
#'                    expression(Psi["leaf"]))
#'   
#'   pData <- data.frame(object) |> 
#'     tidyr::pivot_longer(c(psi, psis:psip_nonlin), 
#'                         names_to = "variable", 
#'                         values_to = "mpa" )
#'   
#'   
#'   pv <- ggplot2::ggplot(pData, aes(x = rwd, y = mpa, color = variable)) +
#'     geom_line(lwd = lwd, lty = lty) +
#'     theme_classic() +
#'     labs(x = xlab, y = ylab, color = "Variable", title = main) +
#'     scale_color_discrete(labels = legendNames) +
#'     aes::theme_base(panel_border_color = "grey50") 
#'   
#'   return(pv)
#' }
