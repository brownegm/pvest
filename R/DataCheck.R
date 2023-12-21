#' Check the condition of the data frame
#'
#' @param data A data frame
#'
#' @return Stops evaluation of function
#'
#' @importFrom tibble is_tibble
#' @noRd

InputCheck <- function(data) {
  # check that data is a data.frame "Check that data is not a tibble."
  stopifnot(!tibble::is.tibble(data))
  # check that there is at least 8 values per leaf
  stopifnot(nrow(data) > 8)
}
