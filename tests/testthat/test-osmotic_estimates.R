test_that("slope is negative and the sma_model output is correct class", {
  
  rwc <- seq(0.95, 0.6, length.out = 10)
  psi <- seq( -0.01, -0.95,length.out = 10)
  
  test_df <- as.data.frame(list("rwc"=rwc, "psi"=psi))
  
  wc.index = "rwc"
  wp.index = "psi"
  
  test <- pvest::estpio("rwc","psi")
  mod <-test$sma_mod
  
  expect_lt(mod$slope, 0)
  
  testosm <- pvest::estOsmotic(test_df, wc.index = "rwc", wp.index = "psi")

  #check class attributes
  expect_s3_class(test, "pioEst")
  expect_s3_class(mod, "sma_model")
  
})

test_that("check that the input lengths are the same", {
  rwc <- seq(0.95, 0.6, length.out = 10)
  psi <- seq(0.95, 0.6, length.out = 4)
  
  expect_error(pvest::estpio(rwc, psi))
})

test_that("Check that the errors work", {
  rwc <- seq(0.95, 0.6, length.out = 10)
  psi <- seq(0.95, 0.6, length.out = 10)
  
  test_df <- as.data.frame(list("rwc"=rwc, "psi"=psi))
  
  #rename the columns 
    names(data)[1]<- "fake_col_name"
  
    expect_error(pvest::estOsmotic(
      data = data,
      fw.index = 1,
      wp.index = 2,
      dm.index = 3,
      silent = F
    ))
  
  })
  


