test_that("validate inputs", {

  data <- pvest::quag 

  #wrong column name in the input
  expect_error(estPV(data, species, leaf, fresh.rain, water.potential, hot.weight))
  
  #inspect output
  ah <- estPV(pvest::quag, species, subgrp = leaf, fw = fresh.weight, water.potential, dry.weight, method ="rmse")
  
  expect_error(estPV(pvest::quag, species, subgrp = leaf, fw = fresh.weight, water.potential, dry.weight, method = "justguess"))
  expect_equal(unique(ah[[1]]$species), "quag_gj")
  expect_equal(names(ah[[1]])[3], "leaf")
  expect_equal(length(ah), length(unique(paste0(pvest::quag$species, pvest::quag$leaf))))
  
  
})
