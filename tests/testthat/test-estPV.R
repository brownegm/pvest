test_that("validate inputs", {

  data <- quag 
  
  estPV.default(data, species, subgrp = NULL, fresh.weight, water.potential, dry.weight)
  
  expect_error(estPV.default(data, species, leaf, fresh.rain, water.potential, hot.weight))
  
})
