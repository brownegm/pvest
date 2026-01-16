# Functions ---------------------------------------------------------------
#
find_outlier <- function(x, y, index) {
  temp <- merge(x, y)

  var <- names(temp)[index]

  temp_output <- temp %>%
    filter({{ index }} <= lwr | {{ index }} >= upr)

  return(temp_output)
}
#
# # for simplicity in later prediction interval estimation this function creates a new df with just the variable
# # to be worked on
f <- function(data, var) {
  new_preds <- data %>% # create df with just the variable of interest
    select(any_of(var)) %>%
    transmute(
      var = seq(
        min(data[, var], na.rm = T),
        max(data[, var], na.rm = T),
        length.out = nrow(data)
      )
    )

  names(new_preds)[1] <- {
    {
      var
    }
  } # rename that column to match

  return(new_preds)
}

rmse <- function(actual, predicted) {
  return(sqrt(mean(se(actual, predicted))))
}
