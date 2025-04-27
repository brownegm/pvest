#' Determine best number of points below turgor loss
#'
#' @description
#' This function aims to determine the best number of points by coefficients of determination, coefficients of variation, and estimated leaf osmotic potential at full turgor.
#'
#' @param data A data frame
#' @param wp Number or string containing the column name of the water potential data
#' @param fw Index number or string containing the column name of the water mass data (i.e., water mass or relative water content)
#' @param dm Index number or string containing the column name of the dry mass data
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
  data$lw <- data[[fw]] - data[[dm]]
  # Define objective function for optimization
  mod_perform_test <- function(threshold) {
    data_above <- data[c(1:threshold), ]
    data_below <- data[c(threshold:nrow(data)), ]
   
    # Count points
    n_above <- nrow(data_above)
    n_below <- nrow(data_below)
    
    # Check that both segments have enough data points
    if (n_above < 4 | n_below < 4)
      return(Inf)  # Penalize small groups
    
    # Fit models using your function
    fit_above <- sma_model(x = data_above[[wp]], y = data_above$lw)
    fit_below <- sma_model(x = data_below[[wp]], y = data_below$lw)
    fit_all <- structure(list(above = fit_above[["m"]], 
                         below = fit_below[["m"]]),
                         method = method,
                         pmetric = NULL,
                         class = "sma_model2")
    # Use RMSE for minimization, R² for maximization
    performance_metric <- switch(
      method,
      "rmse" = attr(fit_below, "rmse") + attr(fit_above, "rmse"),
      "r2" = -(attr(fit_below, "r_squared") + attr(fit_above, "r_squared")),
      # maximize
      "aicc" = attr(fit_below, "aicc") + attr(fit_above, "aicc")
    )
    
    # set combined model objval attribute 
    attr(fit_all, "pmetric") <- performance_metric
    
    # Store the number of points
    attr(performance_metric, "below_above") <- c(n_above, n_below)
    attr(performance_metric, "model") <- fit_all#combined models
    
    invisible(performance_metric)
  }
  
  # Optimize over threshold range
  init_row_below <- median(1:nrow(data))|>round()
  max_row <- nrow(data)
  
  objvals <- lapply(c(init_row_below:max_row), \(n) mod_perform_test(threshold = n))
  testvals_vec <- unlist(objvals)
  
  # Get optimal threshold
  lowest_vec_idx <- which.min(testvals_vec)
  lowest <- objvals[[lowest_vec_idx]]
  
  # Get final point counts
  above_lowest <- lowest[1]
  below_lowest <- lowest[2]
  
  # Get final model
  mod <-  attr(lowest, "model")
  # return output
  return(structure(list(
    est_objvals = objvals,
    lowest = lowest,
    n_above = above_lowest,
    n_below = below_lowest
  )))
}

#' Check and apply optimization for method
#' 
#' @param data Data frame containing leaf fresh water mass and leaf water potentials
#' @param input_cols A vector of column names or indices for the data frame. The second, third, and fourth elements should correspond to fresh weight, water potential, and dry mass respectively.
#' @param method Method to optimize the number of points above and below turgor loss point. Options are "rmse", "aicc", or "r2".
#' 
#' @returns A list containing the number of points above and below turgor loss point.
#' 
apply_optim <- function(data, input_cols, method=NULL) {
  # Check if the method is valid
  if (!method %in% c("rmse", "aicc", "r2", NULL)) {
    stop("Invalid method specified. Choose from 'rmse', 'aicc', 'r2', or leave empty for default method.")
  }
  
  optim <- switch(
    method,
    "rmse" =
      optim_thres(
        data,
        fw = input_cols[2],
        wp = input_cols[3],
        dm = input_cols[4],
        method = "rmse"
      )
    ,
    "aicc" =
      optim_thres(
        data,
        fw = input_cols[2] ,
        wp = input_cols[3] ,
        dm = input_cols[4],
        method = "aicc"
      ),
    "r2" =
      optim_thres(
        data,
        fw = input_cols[2] ,
        wp = input_cols[3] ,
        dm = input_cols[4],
        method = "r2"
      ),
    "default" = NA
  )
  
  return(list(attr(optim$lowest, "below_above"), 
              model = attr(optim$lowest, "model")))
}



#' Print method for SMA model with two segments 
#' @export
#' @param x An object of class `sma_model2`
#' @param ... Additional arguments (not used)
#' @return Prints the model summary

print.sma_model2 <- function(x, ...) {
  cat("SMA Model with two segments\n")
  cat("Above TLP:\n")
  print(x$above)
  cat("\nBelow TLP:\n")
  print(x$below)
  cat("\nMethod used:", x$method, "\n")
  cat("Performance metric:", x$pmetric, "\n")
}

# maybe do this later
# #   } else if (method == "pio") {
#     pio <- lapply(1:length(mods), function(mod) {
#       pio_vals <- -1 / (mods[[mod]]$coef[[1]][1, 1])
#
#     })
