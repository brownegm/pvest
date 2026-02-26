# Determine number of points for estimates that are not one to one.

library(ggplot2)
library(pvest)
library(woodcut)

# Load predicted values  -------------------------------------------------

# load combined data and package estimates
load(here("exec/pkgTest", "combined_pv_estimates.rda"))

plot_comp_cols <- function(data, x, y) {
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(
      aes(fill = as.factor(n_zero)), #color points by the number of points below tlp
      pch = 21,
      size = 5,
      col = "black",
      alpha = 0.6
    ) +
    geom_abline(slope = 1, intercept = 0, lwd = 1.2) +
    geom_smooth(
      method = "lm",
      col = "#4f8359",
      lwd = 1.2,
      linetype = "dashed",
      se = T
    ) +
    labs(x = "Estimate", y = "Original") +
    theme_woodcut(font_size = 24) +
    ggpmisc::stat_poly_eq(
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~")),
      formula = y ~ x,
      parse = TRUE,
      size = 5
    )
  # )+
  # scale_fill_woodcut(palette = "earth_cb", discrete = T)

  return(p)
}

# Plot comparison color by n points ---------------------------------------

com <- combined_manual_auto_estimates # for simplicity in naming
com_n_zeros <- com |>
  group_by(ids) |>
  mutate(n_zero = sum(prespot == 0, na.rm = TRUE)) |>
  filter(!n_zero == 0)


pcpv2 <- plot_comp_cols(com_n_zeros, "pio", "pi_o") +
  labs(title = expression(pi[o]))

pcpv3 <- plot_comp_cols(com_n_zeros, "pi_tlp", "psi_tlp") +
  labs(title = expression(pi[tlp]))

pcpv4 <- plot_comp_cols(com_n_zeros, "af_est", "af") +
  labs(title = "Apoplastic Fraction")

pcpv5 <- plot_comp_cols(com_n_zeros, "rwc_tlp_est", "rwc_tlp") +
  labs(title = "RWC at turgor loss")

# combine them all..
(pcpvall <- patchwork::wrap_plots(
  pcpv2,
  pcpv3,
  pcpv4,
  pcpv5,
  ncol = 4,
  axis_titles = "collect",
  guides = "collect"
))

ggsave(
  filename = here("inst/extdata/pv_color_n_zeros.pdf"),
  pcpvall,
  dpi = 500,
  width = 10,
  height = 5,
  units = "in"
)


# look at codi2

codi2 <- com |>
  filter(ids == "codi_6")


(codi2_hof <- codi2 |>
  ggplot2::ggplot(aes(x = rwd, y = water.potential)) +
  geom_point())


com <- combined_manual_auto_estimates

com_err <- com |>
  mutate(
    pio_err = pio - pi_o,
    pitlp_err = pi_tlp - psi_tlp,
    pio_rmse = sqrt(pio_err^2),
    pitlp_rmse = sqrt(pitlp_err^2),
  ) |>
  arrange(desc(abs(pio_err))) |>
  select(
    spcind,
    pio,
    pi_o,
    pio_err,
    pio_rmse,
    pi_tlp,
    psi_tlp,
    pitlp_err,
    pitlp_rmse
  )
