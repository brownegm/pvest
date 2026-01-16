# Plot dataframe with the combined manual and package estimates

library(ggplot2)
library(pvest)

# 1. Function to create comparisons of data ------------------------------

plot_comp <- function(data, x, y) {
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(
      pch = 21,
      size = 5,
      col = "black",
      fill = "#4f8359",
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
    theme_classic(base_size = 24) +
    ggpmisc::stat_poly_eq(
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~")),
      formula = y ~ x,
      parse = TRUE,
      size = 5
    )

  return(p)
}

# 2. Produce plots for each pressure volume curve parameter ----------------

# load combined data and package estimates
load(here("exec/pkgTest", "combined_pv_estimates.rda"))

save = FALSE # should the plots be save as png

com <- combined_manual_auto_estimates # for simplicity

pcpv1 <- plot_comp(com, "swc_est", "swc") +
  labs(title = "Sat. Water Content")

pcpv2 <- plot_comp(com, "pio", "pi_o") +
  labs(title = expression(pi[o]))

pcpv3 <- plot_comp(com, "pi_tlp", "psi_tlp") +
  labs(title = expression(pi[tlp]))

pcpv4 <- plot_comp(com, "af_est", "af") +
  labs(title = "Apoplastic Fraction")

pcpv5 <- plot_comp(com, "rwc_tlp_est", "rwc_tlp") +
  labs(title = "RWC at turgor loss")

pcpv6 <- plot_comp(com, "modulus_est", "modulus") +
  labs(title = "Modulus")

pcpv7 <- plot_comp(com, "cap_bulk_ft", "cap_ft") +
  labs(title = "Capacitance at FT")

pcpv8 <- plot_comp(com, "cap_bulk_tlp", "cap_tlp") +
  labs(title = "Capacitance at TLP")

pcpv9 <- plot_comp(com, "cap_sym_tlp", "cap_tlp_sym") +
  labs(title = "Capacitance at TLP (sym)")

# Combine plots
pcpvall <- patchwork::wrap_plots(
  pcpv1,
  pcpv2,
  pcpv3,
  pcpv4,
  pcpv5,
  pcpv6,
  pcpv7,
  pcpv8,
  pcpv9,
  ncol = 3,
  axis_titles = "collect"
)

if (save == FALSE) {
  ggsave(
    here("inst/extdata", "pv_params_comparison_nonlin.png"),
    pcpvall,
    width = 14,
    height = 10,
    dpi = 500
  )
}

# 3. Create plots of PV curve data ---------------------------------------

pdf(file = here("exec/pkgTest", "hofler_diagrams.pdf"))

pvest::plotPV(testPV)

dev.off()
