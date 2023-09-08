#' Estimate all pressure volume curve parameters for all leaves
#'
#' @param data A data frame. Input data frame with the raw values
#' @param fw.index A numeric. Data frame index for the water content information.
#' @param wp.index A numeric. Indicates the index of the data frame including the water potential data
#' @param dm.index Numeric index of dry mass data. 
#' @param method Default "cv"; See details for more information. If method="cv", the number of rows to use for predicting pio is based on the lowest number of rows with a CV less than 10%. 
#' @param max_row Defaults to length of the input data frame. Maximum number of rows to check with method. 
#' @details
#'     Data needs to have species or individual information as well as leaf identifiers.
#'     The function takes your "water.potential" column(recommend naming it thus) and a "fresh water content"(again, ideally named fresh.water.content) for multiple
#'     hydration states. For information on the production of leaf pressure volume curves see Sack 2011.
#'     With the data for each leaf it takes the first for values and estimates a saturated water content, estimates
#'     relative water content and then with the relationship between relative water deficit and 1/psi, it estimates the
#'     osmotic potential and pressure potential at full turgor(max.psip) from which the same parameters are estimated for each
#'     subsequent hydration state.
#'     
#'     Method options: 
#'     * "cv" - number of rows chosen by lowest number of points where slope of inv_psi vs RWD is greater than 10%
#'     * "r2" - number of rows chosen as the number of points where R^2 is greatest.
#'     * "lowest" - number of rows chosen as number of points that estimates the lowest pio value between 4-max_rows rows. 
#'     * "median" - number of rows chosen as number of points that estimates the median pio value between 4-max_rows rows. 
#'     * if nothing specified - estimate pio with only 4 observations. 
#'
#' @return Returns a data frame with the estimated parameters
#'
#' @importFrom purrr reduce
#' @import dplyr
#' @export
#'
estParams <- function(data, fw.index, wp.index, dm.index, method="cv", max_row=nrow(data)) {
  # stopifnot()#add check if the data frame is a dataframe
  # create unique ID and add inverse psi
  data$unique_id <- paste(data$species, data$leaf, sep = "_")
  data$inv.water.potential <- -(1 / (data[[wp.index]]))

  unique_ids <- unique(data$unique_id)

  output_est <- list() # list of estimates for each unique id

  for (i in unique_ids) {
    
    leaf_estimate <- data[data$unique_id == i, ]

    # fw.index=water mass, wp.index= water potential
    swc_swm_est <- SaturatedWaterContent(leaf_estimate, fw.index = fw.index, wp.index = wp.index, dm.index = dm.index)

    leaf_estimate[, "saturated.water.mass"] <- swc_swm_est[[1]]

    leaf_estimate[, "saturated.water.content"] <- swc_swm_est[[2]]

    leaf_estimate[, c("relative.water.content", "relative.water.deficit")] <- RelativeWaterCD(leaf_estimate, fw.index = fw.index)

    test.vec <- check_n_pts(leaf_estimate, wp.index="inv.water.potential", wm.index="relative.water.deficit", max_row = max_row)
    
    if(method=="cv"){
      
      cv.vec <- test.vec$cv10
      
      if(is.na(pts.vec[1])){ # sometimes it is the case that none of the estimated cv values are < 10%; if this is the case set as 4
        
        pts = 4
  
      }else{
        
      pts = pts.vec[1]
      
      }
      
    }else if(method=="r2"){
      
      pts = test.vec$r2
        
    }else if(method=="lowest"){
      
      pts = test.vec$pi_o
      
    }else if(method=="median"){

      pts = which(test.vec$all_pio==median(test.vec$all_pio, na.rm = T))
      
      stopifnot(is.numeric(pts))

    }else{
      
      pts = 4
      
    }

  ## need to change the amount of points included for estimation after TLP is estimated.
    tlp_est <- OsmoticEstimates(data = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row = pts)%>%
      EstimateTLP(., wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row_below=pts) %>%
      dplyr::pull(leaf.waterpotential.attlp) %>%
      unique()
    
    ## n rows above and below this tlp estimate
    row_above_tlp <- leaf_estimate %>%
      dplyr::filter(.[, wp.index] > tlp_est) %>%
      nrow()
    row_below_tlp <- leaf_estimate %>%
      dplyr::filter(.[, wp.index] < tlp_est) %>%
      nrow()

    leaf_estimate <- OsmoticEstimates(data = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row = row_below_tlp) # osmotic variables are estimated based on inv.psi vs RWD below TLP

    leaf_estimate <- EstimateTLP(df = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row_above = row_above_tlp, n_row_below = row_below_tlp)

    # estimate other parameters:capacitance above and below tlp.

    leaf_estimate_cap <- capacitance_fttlp(df = leaf_estimate, wc.index = "relative.water.content", s_wc.index = "sym.rwc", wp.index = "water.potential")

    # check this but should maybe work
    leaf_estimate[, "cap.ft.bulk"] <- rep(leaf_estimate_cap[1], nrow(leaf_estimate))
    leaf_estimate[, "cap.ft.sym"] <- rep(leaf_estimate_cap[2], nrow(leaf_estimate))
    leaf_estimate[, "cap.tlp.bulk"] <- rep(leaf_estimate_cap[3], nrow(leaf_estimate))
    leaf_estimate[, "cap.tlp.sym"] <- rep(leaf_estimate_cap[4], nrow(leaf_estimate))

    output_est[[i]] <- leaf_estimate
  }
  # combine all leaf estimates into one data frame.
  output_df <- as.data.frame(purrr::reduce(output_est, rbind))

  return(output_df)
}

