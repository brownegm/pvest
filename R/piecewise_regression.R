# Assuming your data is a dataframe named 'df'
# Replace 'x' and 'y' with your actual column names

# Function to fit a linear model to a subset of data
fit_linear_model <- function(start_idx, end_idx) {
  subset_data <- df[start_idx:end_idx, ]
  return(lm(inv.water.potential ~ relative.water.deficit, data = subset_data))
}

# Function to assess the fit of a linear model
assess_fit <- function(model) {
  # Use a relevant metric, e.g., R-squared
  return(summary(model)$r.squared)
}


# Iterate through possible segment divisions
piecewise_reg <- function(df){

best_fit <- NULL
best_metric <- 0

for (i in 2:(nrow(df) - 1)) {
  model1 <- fit_linear_model(1, i)
  model2 <- fit_linear_model(i + 1, nrow(df))
  
  metric1 <- assess_fit(model1)
  metric2 <- assess_fit(model2)
  
  # Use a metric to compare the fits (e.g., sum of R-squared values)
  total_metric <- metric1 + metric2
  
  if (total_metric > best_metric) {
    best_metric <- total_metric
    best_fit <- list(model1 = model1, model2 = model2)
  }
}

return(best_fit)

}

# Visualize the best fit
plot(leaf_estimate$relative.water.deficit, leaf_estimate$inv.water.potential, main = "Piecewise Linear Regression", xlab = "X", ylab = "Y", col = "blue")
abline(best_fit$model1, col = "red", lwd = 2)
abline(best_fit$model2, col = "green", lwd = 2)
