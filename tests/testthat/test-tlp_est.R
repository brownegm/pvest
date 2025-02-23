test_that("test tlp values same across versions", {
  # expected values
  tlp <- -2.579838
  rwctlp <- 82.3598
  mod <- 7.886113

  # example from CODI1
  psi <- c(
    -0.37, -0.883, -1.188,
    -1.698, -1.87, -2.175,
    -2.523, -2.863, -2.925,
    -4.132, -4.253
  )
  rwc <- c(
    97.665485, 92.887400, 91.282819,
    89.642581, 85.684615, 84.115691,
    82.404138, 81.120474, 79.515893,
    75.201353, 73.668087
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
  wc.index <- "rwc"

  ## default
  default_obj <- estTLP(
    data = data, wp.index = wp.index, wc.index = wc.index,
    n_row_below = n_row_below, n_row_above = n_row_above
  )
  expect_equal(round(default_obj$pi_tlp,6), tlp)
  expect_equal(round(default_obj$rwc_tlp,6), rwctlp)
  expect_equal(round(default_obj$modulus,6), mod)
  ## osm_est
  # create test osmotic object
  osm_obj <- estOsmotic(data, "rwc", "psi", n_row = n_row_below)

  osmest_obj <- estTLP(osm_obj, n_row_above = n_row_above)
  expect_equal(round(osmest_obj$pi_tlp,6), tlp)
  expect_equal(round(osmest_obj$rwc_tlp,6), rwctlp)
  expect_equal(round(osmest_obj$modulus,6), mod)
  expect_identical(osmest_obj, default_obj)
  
  expect_snapshot(print(osmest_obj))
})
