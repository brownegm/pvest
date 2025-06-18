StatPlateauModel <- ggplot2::ggproto("StatPlateauModel", ggplot2::Stat,
                                     required_aes = c("wp", "lw"),
                                     compute_group = function(data, scales) {
                                       bp <- attr(data, "breakpoint")
                                       #if (is.null(bp)) stop("No breakpoint provided for group: ", group_id)
                                       data_split <- pvest:::split_df(data, bp)
                                       fits <- pvest:::sma_model2(data_split, wp = "wp", lw = "lw")
                                       
                                       above <- data.frame(
                                         x = fits$above$data$`above[[lw]]`,
                                         y = fits$above$fitted, 
                                         segment = "above"
                                       )
                                       below <- data.frame(
                                         x = fits$below$data$`below[[lw]]`,
                                         y = fits$below$fitted, 
                                         segment = "below"
                                       )
                                       
                                       rbind(above, below)
                                     }
)

stat_plateau_model <- function(mapping = NULL, data = NULL, geom = "line",
                               position = "identity", na.rm = FALSE,
                               model_fun = sma_model2, breakpoints, ...) {
  ggplot2::layer(
    stat = StatPlateauModel, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = FALSE, inherit.aes = TRUE,
    params = list(...)
  )
}

default[[1]]->s

facet_var <- "leaf"

p1 <- ggplot(s, aes(x = fresh.weight, y = water.potential)) +
  geom_point(size = 3) +
  stat_plateau_model(color = "forestgreen", linewidth = 1.3)# +
  #theme_classic(base_size = 16) +
  #labs(title = "Plateau Effect", x = "Water Mass (g)", y = expression(Psi[leaf]~"(MPa)"))
