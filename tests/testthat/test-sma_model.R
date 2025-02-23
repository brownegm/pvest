test_that("Check input values for NAs", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3, 4, NA)
  z <- c(1, 2, 3, 4, 5)
  oinput <- pvest:::osminput( x, y)
  tinput <- pvest:::tlpinput(x, y, z)

  expect_error(sma_model(x, y), "sma_model:Missing values found in input data. Ensure that there are no missing values.")

  expect_error(sma_model.osm_input(x = oinput), "sma_model:Missing values found in input data. Ensure that there are no missing values.")

  expect_error(sma_model.tlp_input(x = tinput), "sma_model:Missing values found in input data. Ensure that there are no missing values.")
})

test_that("Return expected model parameters", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3, 4, 5)
  model <- sma_model(x = x, y = y)

  expect_equal(model$slope, 1)
  expect_equal(model$intercept, 0)
})
