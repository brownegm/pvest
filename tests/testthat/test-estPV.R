test_that("validate inputs", {
  #wrong column name in the input
  expect_error(estPV(
    data = pvest::quag,
    species,
    leaf,
    fresh.rain,
    water.potential,
    hot.weight
  ))

  #inspect output
  default <- estPV(
    pvest::quag,
    species,
    subgrp = leaf,
    fw = fresh.weight,
    water.potential,
    dry.weight
  )

  expect_equal(length(default), 5) # all things are outputted

  expect_error(estPV(
    pvest::quag,
    species,
    subgrp = leaf,
    fw = fresh.weight,
    water.potential,
    dry.weight,
    method = "justguess"
  ))
  expect_equal(unique(default[[1]]$species), "quag_gj")
  expect_equal(names(default[[1]])[3], "leaf")
  expect_equal(
    length(default),
    length(unique(paste0(pvest::quag$species, pvest::quag$leaf)))
  )

  #check method use
  rmse <- estPV(
    pvest::quag,
    species,
    subgrp = leaf,
    fw = fresh.weight,
    water.potential,
    dry.weight,
    method = "rmse"
  )
  r2 <- estPV(
    pvest::quag,
    species,
    subgrp = leaf,
    fw = fresh.weight,
    water.potential,
    dry.weight,
    method = "r2"
  )
  aicc <- estPV(
    pvest::quag,
    species,
    subgrp = leaf,
    fw = fresh.weight,
    water.potential,
    dry.weight,
    method = "aicc"
  )
})
