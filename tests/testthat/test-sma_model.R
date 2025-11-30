test_that("Check input values for NAs", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3, 4, NA)
  z <- c(1, 2, 3, 4, 5)
  a <- c(1, 2, 3, 4, 5)
  b <- c(1, 2, 3, 4, 5)
  c <- c(1, 2, 3, 4, 5)
  a1 <- c(1, 2, 3, 4, 5)
  b1 <- c(1, 2, 3, 4, 5)
  c1 <- c(1, 2, 3, 4, 5)
  oinput <- pvest:::new_osminput(x, y)
  tinput <- pvest:::tlpinput(x, y, z, a, b, c, a1, b1, c1)

  expect_error(
    sma_model(x, y),
    "sma_model:Missing values found in input data. Ensure that there are no missing values."
  )

  expect_error(
    sma_model.osm_input(x = oinput),
    "sma_model:Missing values found in input data. Ensure that there are no missing values."
  )

  expect_error(
    calc_param_tlp(x = tinput),
    "calc_param_tlp:Missing values found in input data. Ensure that there are no missing values."
  )

  #expect_snapshot(print(sma_model(x,z)))
})

test_that("Return expected model parameters", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3, 4, 5)
  model <- sma_model(x = x, y = y)

  expect_equal(model$slope, 1)
  expect_equal(model$intercept, 0)
})
