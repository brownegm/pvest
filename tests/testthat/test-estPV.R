test_that("validate inputs", {

  data <- pvest::quag 
  
  estPV.default(data, species, subgrp = leaf, fw = fresh.weight, water.potential, dry.weight, n_pts=F)
  
  expect_error(estPV.default(data, species, leaf, fresh.rain, water.potential, hot.weight))
  
})
