#' SWC estimations 
#' @description Estimate the saturated water content from the intercept of the
#'     the relationship between leaf water mass and leaf water potential.
#'
#' @param data Data frame containing leaf fresh water mass and leaf water potentials
#' @param fw.index Numeric value indicating the column number where the leaf water mass data is within data frame
#' @param wp.index Numeric value indicating the column number where the leaf water potential data is within the data frame
#'
#' @details This function calculates the saturated water content as the standard major axis intercept.
#'
#' @seealso
#' * [sma_intercept] Function used for estimating the SMA intercept
#' * [sma_slope] Function used for estimating the SMA slope
#' * [RelativeWaterCD] Estimates the relative water content and relative water deficit using the saturated water content estimate here
#'
#' @family abovetlp
#' 
#' @return Returns the saturated water content from the relationship between water mass and water potential
#' @usage SaturatedWaterContent(data, 1, 2)

SaturatedWaterContent <- function(data, fw.index, wp.index) {
  slope <- sma_slope(x = data[, fw.index], y = data[, wp.index])

  intercept <- sma_intercept(x = data[, fw.index], y = data[, wp.index], slope = slope)

  saturated.water.content <- intercept

  return(saturated.water.content)
}

NULL
#' RWC and RWD estimation
#' @description Estimate the relative water content(RWC) and relative water deficit(RWD) based on the
#'     saturated water content
#'
#' @inheritParams SaturatedWaterContent
#'
#' @return Returns the input dataframe with two new columns containing the relative water content(RWC) for each measurement point and
#'    the relative water deficit(i.e, 100-RWC)
#'
#' @export RelativeWaterCD
#' @family abovetlp

RelativeWaterCD <- function(data, fw.index, wp.index) {
  saturated.water.content <- SaturatedWaterContent(data, fw.index, wp.index)

  data$saturated.water.content <- saturated.water.content
  data$relative.water.content <- (data[, fw.index] / data$saturated.water.content) * 100
  data$relative.water.deficit <- 100 - (data$relative.water.content)

  return(data)
}
