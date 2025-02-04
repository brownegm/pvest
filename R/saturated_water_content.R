#' Saturated water content(SWC) estimation
#' @description Estimate the saturated water content from the intercept of the
#'     the relationship between leaf water mass and leaf water potential. For a
#'     detailed explanation of the function see \code{vignette('swc-and-rwc-estimation')}
#'
#' @param data Data frame containing leaf fresh water mass and leaf water potentials
#' @param fw.index Numeric value indicating the column number where the leaf water mass data is within data frame
#' @param wp.index Numeric value indicating the column number where the leaf water potential data is within the data frame
#' @param dm Leaf dry mass
#' @param n_row A number. Indicates number of rows to estimate parameters from within data
#' @details This function calculates the saturated water content as the standard major axis intercept.
#'
#' @seealso
#' * [sma_intercept()] Function used for estimating the SMA intercept
#' * [sma_slope()] Function used for estimating the SMA slope
#' * [estRWC()] Estimates the relative water content and relative water deficit using the saturated water content estimate here
#'
#' @family abovetlp
#'
#' @return Returns the saturated water content from the relationship between water mass and water potential
#' @usage SaturatedWaterContent(data, fw.index, wp.index, dm.index, n_row = 4)
#'
#' @export
#' 
#' @importFrom dplyr arrange slice_head

estsatwater <- function(data, fw.index, wp.index, dm, n_row = 4) {
  # first select the first for values and estimate SWC and RWC values
  data_abovetlp <- data %>%
    dplyr::arrange(desc({{ wp.index }})) %>%
    dplyr::slice_head(n = n_row) %>%
    as.data.frame()
  
  # dry mass 
  dry_mass <- dm
  
  # estimate the SMA model
  sma_abovetlp <- sma_model(data_abovetlp[, fw.index], data_abovetlp[, wp.index])

  #estimate the saturated water content
  sat_water_mass <- sma_abovetlp$intercept

  sat_water_content <- sat_water_mass / dry_mass

  return(list("swm" = sat_water_mass, "swc" = sat_water_content))
}

NULL
#' RWC and RWD estimation
#' @description Estimate the relative water content(RWC) and relative water deficit(RWD) based on the
#'     saturated water content. For a
#'     detailed explanation of the function see \code{vignette('swc-and-rwc-estimation')}
#'
#' @param data Data frame containing leaf fresh water mass and leaf water potentials
#' @param fw.index Numeric value indicating the column number where the leaf water mass data is within data frame
#' @param wp.index Water potential column index
#' @param dm.index Dry mass column index
#' @param silent Silence printing of column names
#' 
#' @return Returns a list of two containing the relative water content(RWC) for each measurement point and
#'    the relative water deficit(i.e, 100-RWC)
#'
#' @export estRWC
#' @family abovetlp

estRWC <- function(data, fw.index, wp.index, dm.index, n_row = 4, silent) {
  
  varnames <- list("fw" = names(data)[fw.index], "wp" = names(data)[wp.index])
  check_var <- all(varnames %in% names(data))

  if (nrow(data) < n_row){
    stop("The number of rows to estimate parameters from is greater than the number of rows in the data.")
  }
  
  if (check_var == FALSE){
    stop("The column names provided do not exist in the data frame.")
  }
 
  if(silent == FALSE){
    
    cat(
      varnames$fw,
      varnames$wp,
      labels = paste("{Fresh mass, water potential column names}: \n"),
      fill = TRUE
    )
  }
  
  # inputs
  fresh_mass <- data[, varnames$fw]
  dry_mass <- data[, varnames$wp]
  
  #calculate SWC and relative water content
  swc <- estsatwater(
    data,
    fw.index = varnames$fw,
    wp.index = varnames$wp,
    dm = dry_mass,
    n_row = n_row
  )$swc
  
  relative.water.content <- (fresh_mass/ swc) * 100
  relative.water.deficit <- 100 - (relative.water.content)

  plateau <- sum(relative.water.content>100)
  
  rwc.rwd <- structure(
    list(swc, relative.water.content, relative.water.deficit),
    .Names = c("swc", "rwc", "rwd"),
    .flag = paste("There are", plateau, "potential plateau points.")
  )

  return(rwc.rwd)
}
