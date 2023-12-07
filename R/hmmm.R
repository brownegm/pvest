library(boot)

#set.seed(1994)


# mse function 
mse <- function(data, indices) { 
  
  train <- data[indices,]

  model <- lm (y~x, data = train)
  
  y_pred <- predict ( model, newdata = data[-indices, ])
  
  mean((data$y[-indices] -y_pred)^2)
  
}


leaf_estimate

k <- 10

# perform cross validation fro different numbers of points  
cv_results <- sapply(1:nrow(leaf_estimate), function (i) {
  d <- leaf_estimate[,c("relative.water.deficit", "inv.water.potential")]
  cv <- boot::cv.glm(d[c(1:i),], glm(d$inv.water.potential~d$relative.water.deficit), K = 3)
  return (cv)#mean(cv$delta)
})

optimal_points <- which.min(cv_results)

