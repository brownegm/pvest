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

test_that("output has correct class and structure", {
  result <- estPV(
    pvest::quag,
    species,
    subgrp = leaf,
    fw = fresh.weight,
    water.potential,
    dry.weight
  )

  expect_s3_class(result, "estPV")
  expect_true(is.list(result))
  expect_equal(length(result), 5)
  expect_named(result)
  # each element should be a data frame
  for (elem in result) {
    expect_true(is.data.frame(elem))
  }
})

test_that("output attributes are set correctly", {
  result <- estPV(
    pvest::quag,
    species,
    subgrp = leaf,
    fw = fresh.weight,
    water.potential,
    dry.weight
  )

  expect_false(is.null(attr(result, "creation_time")))
  expect_true(inherits(attr(result, "creation_time"), "POSIXct"))
  expect_false(is.null(attr(result, "units")))
  expect_true(is.character(attr(result, "units")))

  # each element should have a breakpoint attribute
  for (elem in result) {
    bp <- attr(elem, "breakpoint")
    expect_false(is.null(bp))
    expect_length(bp, 2)
    expect_true(all(bp > 0))
  }
})

test_that("output data frames have expected columns", {
  result <- estPV(
    pvest::quag,
    species,
    subgrp = leaf,
    fw = fresh.weight,
    water.potential,
    dry.weight
  )

  expected_cols <- c(
    "species", "leaf", "water.potential",
    "invpsi", "pio", "psip_o", "osmpot", "prespot",
    "af", "symrwc", "symrwd", "srwc_tlp", "pi_tlp",
    "rwc_tlp", "rwd_tlp", "modulus", "sym_modulus",
    "cap_bulk_ft", "cap_sym_ft", "cap_bulk_tlp", "cap_sym_tlp"
  )

  for (elem in result) {
    for (col in expected_cols) {
      expect_true(
        col %in% names(elem),
        info = paste("Missing column:", col)
      )
    }
  }
})

test_that("estPV works without subgroup", {
  # quag has only one species, so grouping by species alone should give 1 group
  result <- estPV(
    pvest::quag,
    species,
    fw = fresh.weight,
    wp = water.potential,
    dm = dry.weight
  )

  expect_s3_class(result, "estPV")
  expect_equal(length(result), 1)
})

test_that("invalid method aborts with clear message", {
  expect_error(
    estPV(
      pvest::quag,
      species,
      subgrp = leaf,
      fw = fresh.weight,
      water.potential,
      dry.weight,
      method = "justguess"
    ),
    "Invalid.*method"
  )
})

test_that("per-leaf failure warns and continues", {
  # Create a bad leaf with zero water potentials (causes "cannot invert" error)
  bad_leaf <- data.frame(
    species = "quag_gj",
    leaf = 99,
    dry.weight = rep(0.15, 7),
    water.potential = rep(0, 7),
    fresh.weight = seq(0.20, 0.14, length.out = 7)
  )
  combined <- rbind(pvest::quag, bad_leaf)

  expect_warning(
    result <- estPV(
      combined,
      species,
      subgrp = leaf,
      fw = fresh.weight,
      water.potential,
      dry.weight
    ),
    "Estimation failed for group"
  )

  # bad leaf should have been dropped, original 5 leaves should succeed
  expect_s3_class(result, "estPV")
  expect_equal(length(result), 5)
})

test_that("summary.estPV runs without error", {
  result <- estPV(
    pvest::quag,
    species,
    subgrp = leaf,
    fw = fresh.weight,
    water.potential,
    dry.weight
  )

  expect_output(summary(result), "Estimated PV parameters")
})
