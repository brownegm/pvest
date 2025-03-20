#' Saturated water content(SWC) estimation
#' @description Estimate the saturated water content from the intercept of the
#'     the relationship between leaf water mass and leaf water potential. For a
#'     detailed explanation of the function see \code{vignette('swc-and-rwc-estimation')}
#'
#' @param data Data frame containing leaf fresh water mass and leaf water potentials
#' @param fresh_mass Numeric vector of fresh water mass
#' @param psi Numeric vector of water potential
#' @param dry_mass Leaf dry mass
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
#' @usage estsatwater(fresh_mass, psi, dry_mass)
#'
#' @export
#'
#' @importFrom dplyr arrange slice_head

estsatwater <- function(fresh_mass, psi, dry_mass) {
  # check dry mass
  if (length(dry_mass) > 1) {
    unique_dry_mass <- unique(dry_mass)
    cat("{Unique dry mass values are}:", unique_dry_mass)
    stop("Dry mass should be a single value for a given individual.")
  } else{
    dry_mass <- unique(dry_mass)[1]
  }
  
  
  fw <- fresh_mass
  wp <- psi
  
  # estimate the SMA model
  sma_abovetlp <- sma_model(fw, wp)
  
  #estimate the saturated water content
  sat_water_mass <- sma_abovetlp$intercept
  
  sat_water_content <- sat_water_mass / dry_mass
  
  return(structure(
    list("swm" = sat_water_mass, "swc" = sat_water_content),
    units = list("swm" = "g", "swc" = "g/g")
  ))
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

estRWC <- function(data,
                   fw.index,
                   wp.index,
                   dm.index,
                   n_row = 4,
                   silent = T) {
  nvals <- nrow(data)
  
  if (nvals < n_row) {
    stop(
      "The number of rows to estimate parameters from is greater than the number of rows in the data."
    )
  }
  
  inputtype <- all(is.numeric(c(fw.index, wp.index, dm.index)))
  varnames <- if (inputtype) {
    list(
      "fw" = names(data)[fw.index],
      "wp" = names(data)[wp.index],
      "dm" = names(data)[dm.index]
    )
  } else{
    list("fw" = fw.index,
         "wp" = wp.index,
         "dm" = dm.index)
  }

  if (silent == FALSE) {
    cat("\nEstimating RWC and RWD...\n\n")
    
    print(head(data))
    
    cat(
      "Using the following columns for the estimation:\n",
      "{Fresh mass}: ",
      varnames$fw,
      "\n",
      "{Water potential}: ",
      varnames$wp,
      "\n\n",
      sep = ""
    )
    
  }
  
  # Select values above initial turgor loss guess
  data_abovetlp <- data %>%
    dplyr::arrange(desc({{ wp.index }})) %>%
    dplyr::slice_head(n = nvals - n_row + 1) #rwc is estimated from the values above and including the tlp psi guess
  
  # inputs
  fresh_mass <- data[[varnames$fw]]
  fresh_mass_thres <- data_abovetlp[[varnames$fw]]
  water_potential_thres <- data_abovetlp[[varnames$wp]]
  dry_mass <- unique(data_abovetlp[[varnames$dm]])#if dm.index is an integer send vector of values to estsatwater.
  
  #calculate SWC and relative water content
  satwater <- estsatwater(fresh_mass = fresh_mass_thres,
                          psi = water_potential_thres,
                          dry_mass = dry_mass)
  
  relative.water.content <- (fresh_mass / satwater$swm) * 100
  relative.water.deficit <- 100 - (relative.water.content)
  
  plateau <- sum(relative.water.content > 100)
  
  dataUpd <- do.call(cbind,
                     list(
                       data,
                       satwater,
                       relative.water.content,
                       relative.water.deficit
                     ))
  
  names(dataUpd) <- c(names(data), "swm", "swc", "rwc", "rwd")
  
  out_rwcrwd <- structure(
    list(
      satwater$swm,
      satwater$swc,
      relative.water.content,
      relative.water.deficit
    ),
    .Names = c("swm", "swc", "rwc", "rwd"),
    units = c("g", "g/g", "%", "%"),
    flag = paste("There are", plateau, "potential plateau points."),
    df = dataUpd,
    class = "rwcEst"
  )
  return(out_rwcrwd)
}
