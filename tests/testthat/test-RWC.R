test_that("Plateau points found", {
  data <- data.frame(fw = c(1, 2, 3, 4), wp = c(1, 2, 3, 4), dm = 0.5)
  
  estRWC(data = data, fw.index = 1, wp.index = 2, dm = data$dm, silent = F)
})

test_that("Check the output values are resonable.", {
  
})
