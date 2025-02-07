


sma_model <- S7::new_class(
  "sma_model",
  properties = list(slope = class_numeric, intercept = class_numeric),
  validator = function(self) {
    if (anyNA(slope) | anyNA()) {
 "sma_model:Missing values found in input data. Ensure that there are no missing value"
    }
    )

  methods = list(
    initialize = function(x, y) {
      if (anyNA(x) | anyNA(y)) {
        stop("sma_model:Missing values found in input data. Ensure that there are no missing values.")
      }
      slope <- sd(x) / sd(y)
      intercept <- mean(x) - (slope * mean(y))
      new(x = x, y = y, slope = slope, intercept = intercept)
    }
  )
)