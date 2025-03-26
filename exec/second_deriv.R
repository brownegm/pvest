## calculate second derivative of loess 


#' calculate second derivative
#'
#' @param fit lm fit
#' @param ind_var individual variable
#'
#' @return first and second derivative
#' 
#'
#' @importFrom stats predict
sec_der <- function(fit, ind_var){
  
  y_pred <- predict(fit)
  
  # Calculate the first derivative using numerical differentiation
  loess_derivative <- diff(y_pred) / diff(ind_var)
  
  # Pad the result with NA to match the length of the original data
  loess_derivative <- c(NA, loess_derivative)
  
  # Calculate the second derivative using numerical differentiation
  loess_second_derivative <- diff(loess_derivative) / diff(ind_var)
  
  # Pad the result with NA to match the length of the original data
  loess_second_derivative <- c(NA, loess_second_derivative)
  
  return(list(first_der=loess_derivative, 
              second_der = loess_second_derivative))
  
}
