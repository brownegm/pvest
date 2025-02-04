test_that("Check input values for NAs", {
  expect_error(sma_model(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 4, NA)), "sma_model:Missing values found in input data. Ensure that there are no missing values.")
})

test_that("Return expected model parameters",{
  
  x<- c(1, 2, 3, 4, 5)
  y<- c(1, 2, 3, 4, 5)
  model <- sma_model(x = x, y = y)
  
  expect_equal(model$slope, 1)
  expect_equal(model$intercept, 0)
})
