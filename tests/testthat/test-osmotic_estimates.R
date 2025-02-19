test_that("slope is negative and the sma_model output is correct class", {
  
  psi <- c(-0.37,-0.883,-1.188,
           -1.698,-1.87,-2.175,
           -2.523,-2.863,-2.925,
           -4.132,-4.253)
  rwc <- c(97.67,92.89,91.28,89.64,
           85.68,84.12,82.40,81.12,
           79.52,75.20,73.67)
  
  rwd <- 100-rwc
  
  neg_inv_psi <- -1/psi
  
  # test dataframe
  test_df <- as.data.frame(list("rwc"=rwc,
                                "psi"=psi,
                                "rwd"=rwd,
                                "neg_inv_psi"=neg_inv_psi))

  wp.index = "psi"
  
  test_df_below <- test_df %>%
    dplyr::arrange(desc(wp.index)) %>%
    dplyr::slice_tail(n = 5)
  
  test <- pvest::estpio(test_df_below$rwd,
                        test_df_below$neg_inv_psi)
  
  mod <-test$sma_mod
  
  #check expected results
  expect_equal(mod$intercept, 0.718969368)
  expect_equal(round(mod$slope,7), -0.0187824)
  
  # run full osmotic estimation
  wc.index = "rwc"

  testosm <- pvest::estOsmotic(test_df,
                               wc.index = wc.index, wp.index = wp.index, 
                               n_row=5)

  # check that the wrapper gets the same result
  expect_equal(testosm$pio, test$pio)
  
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
  


