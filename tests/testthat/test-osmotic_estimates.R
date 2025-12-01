testthat::test_that("slope is negative and the sma_model output is correct class", {
  # example from CODI1
  psi <- c(
    -0.37,
    -0.883,
    -1.188,
    -1.698,
    -1.87,
    -2.175,
    -2.523,
    -2.863,
    -2.925,
    -4.132,
    -4.253
  )
  rwc <- c(
    97.665485,
    92.887400,
    91.282819,
    89.642581,
    85.684615,
    84.115691,
    82.404138,
    81.120474,
    79.515893,
    75.201353,
    73.668087
  )
  rwd <- 100 - rwc
  neg_inv_psi <- -1 / psi

  test_df <- data.frame(
    psi = psi,
    rwc = rwc,
    rwd = rwd,
    neg_inv_psi = neg_inv_psi
  )

  wp.index <- "psi"

  test_df_below <- test_df |>
    dplyr::arrange(desc(wp.index)) |>
    dplyr::slice_tail(n = 5)

  test <- pvest::estpio(
    test_df_below$rwd,
    test_df_below$neg_inv_psi
  )

  mod <- test$sma_mod

  # check expected results
  expect_equal(mod$intercept, 0.718842151)
  expect_equal(mod$slope, -0.018776484)

  # run full osmotic estimation
  wc.index <- "rwd"

  testosm <- pvest::estOsmotic(
    test_df,
    wc.index = wc.index,
    wp.index = wp.index,
    n_row = 5
  )

  testosm_numinput <- pvest::estOsmotic(
    test_df,
    wc.index = 2,
    wp.index = 1,
    n_row = 5
  )

  # check that the function throws an error if the input is not correct
  expect_error(pvest::estOsmotic(
    test_df,
    wc.index = "rwc",
    wp.index = 1,
    n_row = 5
  ))

  expect_identical(testosm, testosm_numinput)

  expect_snapshot(print(testosm))
  # check that symplastic rwc is correct.... should be less that bulk rwc
  expect_true(all(testosm$sym_rwc < testosm$rwc))
  # check that the wrapper gets the same result
  expect_equal(testosm$pio, test$pio)
  expect_equal(length(testosm$osmpot), nrow(test_df)) # check that the length is the same as input
  # check class attributes
  expect_s3_class(test, "pioEst")
  expect_s3_class(mod, "sma_model")
})

test_that("nonlinear pressure potential fits with srwc below TLP", {
  psi <- c(
    -0.37,
    -0.883,
    -1.188,
    -1.698,
    -1.87,
    -2.175,
    -2.523,
    -2.863,
    -2.925,
    -4.132,
    -4.253
  )
  rwc <- c(
    97.665485,
    92.887400,
    91.282819,
    89.642581,
    85.684615,
    84.115691,
    82.404138,
    81.120474,
    79.515893,
    75.201353,
    73.668087
  )

  rwd <- 100 - rwc
  neg_inv_psi <- -1 / psi
  test_df <- data.frame(
    psi = psi,
    rwc = rwc,
    rwd = rwd,
    neg_inv_psi = neg_inv_psi
  )

  n_row <- 5
  idx_tail <- (nrow(test_df) - n_row + 1L):nrow(test_df)

  pio <- estpio(test_df$rwd[idx_tail], test_df$neg_inv_psi[idx_tail])
  sym <- calc_symrwc(rwc = test_df$rwc, sma_mod = pio$sma_mod)

  pi_sat <- pio$pio
  pi_vec <- pi_sat / sym$srwc
  psip_lin <- test_df$psi - pi_vec

  rtlp_fit <- sma_model(
    sym$srwc[seq_len(nrow(test_df) - n_row)],
    psip_lin[seq_len(nrow(test_df) - n_row)]
  )
  r_tlp_init <- -rtlp_fit$intercept / rtlp_fit$slope

  psip_mod <- calc_nonlin_psip(
    data = data.frame(r = sym$srwc, psip_linear = psip_lin),
    pi_sat = pi_sat,
    r_tlp = r_tlp_init
  )

  expect_s3_class(psip_mod, "nls")
  expect_true(psip_mod$convInfo$isConv)
  expect_length(predict(psip_mod), nrow(test_df))
})

testthat::test_that("compare the dataframe methods to the rwcEst method", {
  quag1 <- pvest::quag |>
    dplyr::filter(leaf == 1)

  rwc <- pvest::estRWC(
    quag1,
    fw.index = "fresh.weight",
    wp.index = "water.potential",
    dm.index = "dry.weight",
    n_row = 5,
    silent = F
  )

  rwcObjEst <- estOsmotic(rwc, n_row = 5, silent = T)

  forcompare <- attr(rwc, "df")

  rwcDfEst <- pvest::estOsmotic(
    forcompare,
    wc.index = "rwd",
    wp.index = "water.potential",
    n_row = 5,
    silent = T
  )

  expect_identical(rwcObjEst, rwcDfEst)
})

test_that("check that the input lengths are the same", {
  rwc <- seq(0.95, 0.6, length.out = 10)
  psi <- seq(0.95, 0.6, length.out = 4)

  expect_error(pvest::estpio(rwc, psi))
})

test_that("Check that the errors work", {
  rwc <- seq(0.95, 0.6, length.out = 10)
  psi <- seq(0.95, 0.6, length.out = 10)

  test_df <- as.data.frame(list("rwc" = rwc, "psi" = psi))

  # rename the columns

  expect_error(pvest::estOsmotic(
    data = test_df,
    wc.index = "rwc",
    wp.index = "fake_col_name",
    silent = F
  ))

  expect_snapshot(pvest::estOsmotic(
    data = test_df,
    wc.index = "rwc",
    wp.index = "psi",
    silent = T
  ))
})
