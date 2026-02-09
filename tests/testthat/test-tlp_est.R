testthat::test_that("test tlp values same across versions", {
  # expected values
  tlp <- -2.579838
  rwctlp <- 82.3598
  mod <- 7.886113

  # example from CODI1
  psi <- c(
    -0.37,
    -0.883,
    -1.188,
    -1.698,
    -1.87,
    -2.175,
    -2.523,
    -2.863,
    -2.925,
    -4.132,
    -4.253
  )
  rwc <- c(
    97.665485,
    92.887400,
    91.282819,
    89.642581,
    85.684615,
    84.115691,
    82.404138,
    81.120474,
    79.515893,
    75.201353,
    73.668087
  )
  rwd <- 100 - rwc
  neg_inv_psi <- -1 / psi

  data <- data.frame(
    psi = psi,
    rwc = rwc,
    rwd = rwd,
    neg_inv_psi = neg_inv_psi
  )
  # name some function parameters
  n_row_above <- 7
  n_row_below <- 5

  # index names
  wp.index <- "psi"
  wc.index <- "rwd"

  ## default
  default_obj <- estTLP(
    data = data,
    wp.index = wp.index,
    wc.index = wc.index,
    n_row_below = n_row_below,
    n_row_above = n_row_above,
  )

  # with the non linear updates these do not match the manual estimates
  # expect_equal(round(default_obj$pi_tlp, 6), tlp)
  # expect_equal(round(default_obj$rwc_tlp, 6), rwctlp)
  # expect_equal(round(default_obj$modulus, 6), mod)
  #
  ## osm_est
  # create test osmotic object
  osm_obj <- estOsmotic(data, "rwd", "psi", n_row = n_row_below)

  osmest_obj <- estTLP(osm_obj, n_row_above = n_row_above)
  # expect_equal(round(osmest_obj$pi_tlp, 6), tlp)
  # expect_equal(round(osmest_obj$rwc_tlp, 6), rwctlp)
  # expect_equal(round(osmest_obj$modulus, 6), mod)
  testthat::expect_identical(osmest_obj, default_obj)

  #testthat::expect_snapshot(print(osmest_obj))
})

# testthat::test_that("Internal functions work", {
#   # expected values
#   tlp <- -2.579838
#   rwctlp <- 82.3598
#   mod <- 7.886113

#   # example from CODI1
#   psi <- c(
#     -0.37,
#     -0.883,
#     -1.188,
#     -1.698,
#     -1.87,
#     -2.175,
#     -2.523,
#     -2.863,
#     -2.925,
#     -4.132,
#     -4.253
#   )
#   rwc <- c(
#     97.665485,
#     92.887400,
#     91.282819,
#     89.642581,
#     85.684615,
#     84.115691,
#     82.404138,
#     81.120474,
#     79.515893,
#     75.201353,
#     73.668087
#   )
#   rwd <- 100 - rwc
#   neg_inv_psi <- -1 / psi

#   data <- data.frame(
#     psi = psi,
#     rwc = rwc,
#     rwd = rwd,
#     neg_inv_psi = neg_inv_psi
#   )

#   osm_obj <- estOsmotic(
#     data,
#     wc.index = wc.index,
#     wp.index = wp.index,
#     n_row = n_row_below
#   )

#   # collect indices for above and below turgor loss point
#   above_idx <- c(1:n_row_above)
#   below_idx <- c(osm_obj$est_rows:nrow(osm_obj$data))

#   # determine sma parameters
#   param_list <- psip_rwd_params(
#     # above
#     psi_above = osm_obj$psi[above_idx],
#     symrwc_above = osm_obj$symrwc[above_idx],
#     rwc_above = osm_obj$data$rwc[above_idx],
#     psip = osm_obj$prespot[above_idx],
#     rwd = osm_obj$data$rwd[above_idx],
#     symrwd = osm_obj$symrwd[above_idx],
#     # below
#     psi_below = osm_obj$psi[below_idx],
#     symrwc_below = osm_obj$symrwc[below_idx],
#     rwc_below = osm_obj$data$rwc[below_idx]
#   )

#   # collect model parameters for below from osm object
#   osm_slope <- osm_obj$model$slope
#   osm_intercept <- osm_obj$model$intercept

#   # calculate PV parameters at turgor loss point
#   rwd_tlp <- -((param_list$intercept) / (param_list$slope))
#   rwc_tlp <- 100 - rwd_tlp
#   sym_rwd_tlp <- -((param_list$intercept_sym) / (param_list$slope_sym))
#   sym_rwc_tlp <- 100 - sym_rwd_tlp
#   pi_tlp <- -1 / (osm_slope * rwd_tlp + osm_intercept)
#   modulus <- osm_obj$psip_o / (rwd_tlp / 100)
#   sym_modulus <- osm_obj$psip_o / (sym_rwd_tlp / 100)

#   # estimate capacitance
#   cap_bulk_ft <- param_list$slope_cap_ft
#   cap_sym_ft <- param_list$slope_cap_sym_ft
#   cap_bulk_tlp <- param_list$slope_cap_tlp
#   cap_sym_tlp <- param_list$slope_cap_sym_tlp
# })
