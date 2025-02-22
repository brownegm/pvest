test_that("test tlp values same across versions", {
  # expected values
  tlp = -2.58
  rwctlp = 82.357
  mod = 7.885
  
  # example from CODI1 
  data <- data.frame(psi = c(-0.37,-0.883,-1.188,
                             -1.698,-1.87,-2.175,
                             -2.523,-2.863,-2.925,
                             -4.132,-4.253),
                     rwc = c(  97.665485,92.887400,91.282819,
                               89.642581,85.684615,84.115691,
                               82.404138,81.120474,79.515893,
                               75.201353,73.668087), 
                     rwd = 100-rwc, 
                     neg_inv_psi = -1/psi)

  # name some function parameters
  n_row_above <- 7
  n_row_below <- 5
  
  # index names
  wp.index = "psi"
  wc.index = "rwc"

##default
  default_obj <- estTLP(data, wp.index = wp.index, wc.index = wc.index,
                        n_row_below = n_row_below, n_row_above = n_row_above)
  expect_equal(default_obj$pi_tlp, tlp)
  expect_equal(default_obj$rwc_tlp, rwctlp)
  expect_equal(default_obj$modulus, mod)
## osm_est
  # create test osmotic object
  osm_obj <- estOsmotic(data, "rwc", "psi", n_row = n_row_below) 
  
  osmest_obj <- estTLP(osm_obj, n_row_above = n_row_above)
  expect_equal(osmest_obj$pi_tlp, tlp)
  expect_equal(osmest_obj$rwc_tlp, rwctlp)
  expect_equal(osmest_obj$modulus, mod)
  
  
})
