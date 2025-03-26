#' fit_linear_model
#'
#' @description Function to fit a linear model to a subset of data
#'
#' @param df a dataframe 
#' @param start_idx starting index for subset
#' @param end_idx ending index for subset
#' @noRd
#' @return Return model fit for subset


fit_linear_model <- function(df, start_idx, end_idx) {
  subset_data <- df[start_idx:end_idx, ]
  return(lm(inv.water.potential ~ relative.water.deficit, data = subset_data))
}

NULL 

#' Extract_stat: Extract statistic from model fit 
#'
#' @description Function to assess the fit of a linear model
#'
#' @param model a linear model
#' @return Return statistic : R^2
#' @noRd
extract_stat <- function(model) {
  # Use a relevant metric, e.g., R-squared
  return(summary(model)$r.squared)
}
NULL

#' Piecewise linear regression estimate
#'
#' @description
#' Iterate through possible segment divisions
#' 
#' @param df A data.frame containing inverse water potential and relative water deficit
#' @return Returns list of two best fitting models over the two sections of data
#' @importFrom stats lm predict
#' @noRd

piecewise_reg <- function(df){

best_fit <- NULL
best_metric <- 0

for (i in 2:(nrow(df) - 1)) {
  
  model1 <- fit_linear_model(df, 1, i)
  model2 <- fit_linear_model(df, i + 1, nrow(df))
  
  metric1 <- extract_stat(model1)
  metric2 <- extract_stat(model2)
  
  # Use a metric to compare the fits (e.g., sum of R-squared values)
  total_metric <- metric1 + metric2
  
  if (total_metric > best_metric) {
    best_metric <- total_metric
    best_fit <- list(above = model1, below = model2)
  }
}

return(best_fit)

}


