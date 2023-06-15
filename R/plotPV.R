#' plotPV
#'
#' @param data
#' @param x
#' @param y
#' @param rows number of rows to select
#' @param ...
#'
#' @return a plot
#' @export
#'
#' @examples
#' plotPV(data[1:4, "leaf" = 5], x = "fresh.weight", y = "water.potential")
#'
plotPV <- function(data, x, y, rows = 4, ...) {
  slope <- pvest::sma_slope(data[[x]][1:rows], data[[y]][1:rows])
  int <- pvest::sma_intercept(data[[x]][1:rows], data[[y]][1:rows], slope)

  l.x <- seq(0, max(data[[x]]), length.out = length(data[[x]]))
  l.y <- int + (slope * l.x)

  plot(data[[y]] ~ data[[x]],
    ylab = y,
    xlab = x
  )

  lines(x = l.x, y = l.y)
}

# #other thoughts using ggplot
# library(ggplot2)
#
# var1<-"relative.water.deficit"
# var2<-"inv.water.potential"
# group_var<-unique_id
# group_name<-"magr_2"
# rows=4
#
# plot_points_lines <- function(data,
#                               var1, var2,
#                               group_var,group_name,
#                               x2 = NULL,
#                               color1 = "black", color2 = "#a9b79a",
#                               rows=4,...) {
#
#   # create data frame with the x and y coordinates
#   plot.df <- pv_params%>%
#     dplyr::filter(unique_id=={{group_name}})
#
#   row_below_tlp<-plot.df%>%dplyr::filter(.[,"water.potential"]<leaf.waterpotential.attlp)%>%nrow()
#   plot.df.btlp<-plot.df%>%slice_tail(n=row_below_tlp)
#   #estimate slope and intercept
#   slope <- pvest::sma_slope(plot.df.btlp[,var1], plot.df.btlp[,var2])
#
#   intercept <- pvest::sma_intercept(plot.df.btlp[[var1]], plot.df.btlp[[var2]], slope)
#
#   # create basic plot with points
#   plot <- ggplot(plot.df, aes(x=.data[[var1]], .data[[var2]])) +
#     geom_point(shape=21,color = color1,fill=color2, size = 5)+
#     geom_abline(slope=-0.016425336, intercept = 0.6196721)+
#     annotate("text", "topright")
#     theme_base()
#   plot
#
#
#
#   # add line representing linear model
#   line_data <- data.frame(x = range(x))
#   line_data$y <- intercept + slope * line_data$x
#   plot <- plot + geom_line(data = line_data, aes(x, y), color = color2)
#
#   return(plot)
# }
#
