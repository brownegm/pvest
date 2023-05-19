#' Saturated water content(SWC) estimation 
#' @description Estimate the saturated water content from the intercept of the
#'     the relationship between leaf water mass and leaf water potential. For a 
#'     detailed explanation of the function see \code{vignette('swc-and-rwc-estimation')}
#'
#' @param data Data frame containing leaf fresh water mass and leaf water potentials
#' @param fw.index Numeric value indicating the column number where the leaf water mass data is within data frame
#' @param wp.index Numeric value indicating the column number where the leaf water potential data is within the data frame
#' @param dm.index Numeric value indicating the column number where the leaf dry mass is
#' @param n_row A number. Indicates number of rows to estimate parameters from within data
#' @details This function calculates the saturated water content as the standard major axis intercept.
#'
#' @seealso
#' * [sma_intercept()] Function used for estimating the SMA intercept
#' * [sma_slope()] Function used for estimating the SMA slope
#' * [RelativeWaterCD()] Estimates the relative water content and relative water deficit using the saturated water content estimate here
#'
#' @family abovetlp
#' 
#' @return Returns the saturated water content from the relationship between water mass and water potential
#' @usage SaturatedWaterContent(data, fw.index, wp.index, n_row)
#' 
#' @export

SaturatedWaterContent <- function(data, fw.index, wp.index, dm.index,  n_row=4) {
  
  #first select the first for values and estimate SWC and RWC values 
  data_abovetlp<-data%>% 
    dplyr::arrange(desc({{wp.index}}))%>%
    dplyr::slice_head(n=n_row)%>%as.data.frame()
  
  slope <- sma_slope(x = data_abovetlp[, fw.index], y = data_abovetlp[, wp.index])

  intercept <- sma_intercept(x = data_abovetlp[, fw.index], y = data_abovetlp[, wp.index], slope = slope)

  saturated.water.mass <- intercept
  
  saturated.water.content <- intercept / unique(data_abovetlp[,dm.index])

  return(list("SWM"=saturated.water.mass,"SWC"=saturated.water.content))
}

NULL
#' RWC and RWD estimation
#' @description Estimate the relative water content(RWC) and relative water deficit(RWD) based on the
#'     saturated water content. For a 
#'     detailed explanation of the function see \code{vignette('swc-and-rwc-estimation')}
#'
#' @param data Data frame containing leaf fresh water mass and leaf water potentials
#' @param fw.index Numeric value indicating the column number where the leaf water mass data is within data frame
#'
#' @return Returns a list of two containing the relative water content(RWC) for each measurement point and
#'    the relative water deficit(i.e, 100-RWC)
#'
#' @export RelativeWaterCD
#' @family abovetlp

RelativeWaterCD <- function(data, fw.index) {
  relative.water.content <- (data[, fw.index] / data$saturated.water.mass) * 100
  relative.water.deficit <- 100 - (relative.water.content)

  rwc.rwd<-c(relative.water.content, relative.water.deficit)
  
  return(rwc.rwd)
}
