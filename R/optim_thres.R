#' Determine best number of points below turgor loss
#'
#' @description
#' This function aims to determine the best number of points by coefficients of determination, coefficients of variation, and estimated leaf osmotic potential at full turgor.
#'
#' @param data a data frame
#' @param x_var number or string containing the column name of the water potential data
#' @param y_var index number or string containing the column name of the water mass data (i.e., water mass or relative water content)
#' @param method Specify statistic used to optimize the number of rows above and below turgor loss. 

#' @return Returns list with:
#' * the objective cost function value for each index that was used to minimize (i.e., rmse or aicc) or maximized (R squared). The attributes of which are the number of rows on either side of turgor used to fit the standard major axis model. 
#' * Number of rows above (`n_above`) and below (`n_below`) that optimized the objective function. 
#'
#' @family internal
#' @importFrom stats median
#'
#' @export

optim_thres <- function(data, fw, wp, dm, method = "rmse") {
  # create leaf water column
  data$lw <- data[[fw]]- data[[dm]]
  # Define objective function for optimization
  objective_function <- function(threshold) {
    data_low <- data[c(threshold:nrow(data)), ]
    data_high <- data[c(1:threshold), ]
    
    # Count points
    n_low <- nrow(data_low)
    n_high <- nrow(data_high)
    
    # Check that both segments have enough data points
    if (n_low < 4 | n_high < 3)
      return(Inf)  # Penalize small groups
    
    leafWater <- data_
    # Fit models using your function
    fit_low <- sma_model(x = data_low[[wp]], y = data_low$lw)
    fit_high <- sma_model(x = data_high[[wp]], y = data_high$lw)
    
    # Use RMSE for minimization, R² for maximization
    objective_value <- switch(
      method,
      "rmse" = attr(fit_low, "rmse") + attr(fit_high, "rmse"), 
      "r2" = -(attr(fit_low, "r_squared") + attr(fit_high, "r_squared")),# maximize
      "aicc" = attr(fit_low, "aicc") + attr(fit_high, "aicc")
    )
    
    # Store the number of points
    attr(objective_value, "below_above") <- c(n_low, n_high)
    
    invisible(objective_value)
  }
  
  # Optimize over threshold range
  init_row_below <- median(1:nrow(data))
  max_row <- nrow(data)
  
  objvals <- lapply(init_row_below:max_row, \(n) objective_function(threshold = n))
  testvals_vec <- unlist(objvals)
  
  # Get optimal threshold
  lowest_vec_idx <- which.min(testvals_vec)
  lowest <- attr(objvals[[lowest_vec_idx]], "below_above")
  
  # Get final point counts
  n_low <- lowest[1]
  n_high <- lowest[2]
  
  return(list(
    est_objvals = objvals,
    n_below = n_low,
    n_above = n_high
  ))
}
# maybe do this later
# #   } else if (method == "pio") {
#     pio <- lapply(1:length(mods), function(mod) {
#       pio_vals <- -1 / (mods[[mod]]$coef[[1]][1, 1])
#       
#     })
