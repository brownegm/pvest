test_that("Check that the output makes sense", {
  data <- data.frame(fw = seq(0.450,0.2, length.out = 10),
                     wp = seq(-0.5,-2.5, length.out = 10),
                     dm = 0.5)
  
  rwc_estimate <- estRWC(
    data = data,
    fw.index = 1,
    wp.index = 2,
    dm.index = 3,
    silent = F
  )
  
  expect_equal(length(rwc_estimate$swc), 1)
  expect_equal(length(rwc_estimate$rwc), nrow(data))
  expect_equal(length(rwc_estimate$rwd), nrow(data))
  
  # check that the rwc values are increasing 
  rwcdiff <- all(diff(rwc_estimate$rwc) < 0)
  
  expect_true(rwcdiff)
})

