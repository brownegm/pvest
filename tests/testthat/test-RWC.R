# Testing the functionality of the RWC and saturated water content estimation

# simple fake dataset
data <- data.frame(
  fw = seq(0.450, 0.2, length.out = 10),
  wp = seq(-0.5, -2.5, length.out = 10),
  dm = 0.5
)

test_that("Check that the output makes sense", {
  rwc_estimate <- estRWC(
    data = data,
    fw.index = 1,
    wp.index = 2,
    dm.index = 3,
    silent = T
  )

  expect_equal(length(rwc_estimate$swc), 1)
  expect_equal(length(rwc_estimate$rwc), nrow(data))
  expect_equal(length(rwc_estimate$rwd), nrow(data))

  # check that the rwc values are increasing
  # dont change input fake data

  rwcdiff <- all(diff(rwc_estimate$rwc) < 0) #-2.710027
  modediff <- unique(diff(rwc_estimate$rwc) |> round(5)) == -5.42005
  expect_true(rwcdiff)
  expect_true(modediff)
})

test_that("Check that the errors work", {
  expect_error(estRWC(
    data = data,
    fw.index = 1,
    wp.index = 2,
    dm.index = 3,
    n_row = Inf,
    silent = T
  ))

  expect_snapshot(estRWC(
    data = data,
    fw.index = 1,
    wp.index = 2,
    dm.index = 3,
    n_row = 4,
    silent = F
  ))

  data_wrong_dm <- data
  data_wrong_dm[c(3:5), 3] <- c(0.1,0.2,0.3) #create nonsense dry mass values
  
  expect_error(estsatwater(data_wrong_dm[, 1], data_wrong_dm[, 2], data_wrong_dm[, 3]))
  
})
