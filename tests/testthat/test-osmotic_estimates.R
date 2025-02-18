test_that("slope is negative and the sma_model output is correct class", {
  
  psi <- c(-0.37,-0.883,-1.188,-1.698,-1.87,-2.175,-2.523,-2.863,-2.925,-4.132,-4.253)
  rwc <- c(97.67,
           92.89,
           91.28,
           89.64,
           85.68,
           84.12,
           82.40,
           81.12,
           79.52,
           75.20,
           73.67)
  
  test_df <- as.data.frame(list("rwc"=rwc, "psi"=psi))
  
  wc.index = "rwc"
  wp.index = "psi"
  
  test <- pvest::estpio(rwc,psi)
  mod <-test$sma_mod
  
  expect_lte(mod$slope, 0)
  
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
  


