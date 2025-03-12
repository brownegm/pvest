#' Estimate all pressure volume curve parameters for all leaves
#'
#' @param data A data frame. Input data frame with the raw values
#' @param fw.index A numeric. Data frame index for the water content information.
#' @param wp.index A numeric. Indicates the index of the data frame including the water potential data
#' @param dm.index Numeric index of dry mass data.
#' @param n_pts Logical. If `TRUE`, the number of rows to use for predicting pio is based on the lowest number of rows with a CV less than 10%.
#' @param method Optional character parameter for when n_pts=T. Determines which method to pick number of rows below TLP.Options include "r2","pio","cv". See `?check_n_points` for more information
#'
#' @details
#'     Data needs to have species or individual information as well as leaf identifiers.
#'     The function takes your "water.potential" column(recommend naming it thus) and a "fresh water content"(again, ideally named fresh.water.content) for multiple
#'     hydration states. For information on the production of leaf pressure volume curves see Sack 2011.
#'     With the data for each leaf it takes the first for values and estimates a saturated water content, estimates
#'     relative water content and then with the relationship between relative water deficit and -1/psi, it estimates the
#'     osmotic potential and pressure potential at full turgor(max.psip) from which the same parameters are estimated for each
#'     subsequent hydration state.
#'
#' @return Returns a data frame with the estimated parameters
#'
#' @importFrom purrr reduce
#' @import dplyr
#' @import rlang
#' @import cli
#' @export
#'

estPV <- function(data, fw.index, wp.index, dm.index, n_pts, method = NULL, ...) {
  if (n_pts == T & is.null(method)) {
    "Method must be specified when n_pts=T"
  }

  UseMethod("estPV")
}

estPV.default <- function(data, group, subgrp = NULL, fw, wp, dm, n_pts, method = NULL) {
  
  # Convert inputs to quosures for tidy evaluation
  grp <- rlang::enquo(group)
  fw <- rlang::enquo(fw)
  wp <- rlang::enquo(wp)
  dm <- rlang::enquo(dm)
  sbgrp <- rlang::enquo(subgrp)

  # Check that columns exist
  input_cols <- c(as_name(grp), as_name(fw), as_name(wp), as_name(dm))

  cli::cli_alert_info("Grouping data by: {as_name(grp)}")

  missing_cols <- setdiff(input_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("The following columns are missing from the data frame: {missing_cols}")
  }
  # Get optional subgrouping
  if(!is.null(sbgrp) && as_name(sbgrp) %in% names(data)) {
    
  cli::cli_alert_info("Subgrouping data by: {as_name(sbgrp)}")
    
  } else{
    
  cli::cli_abort("Specified subgroup ({as_name(sbgrp)}) does not exist in the data frame.")
  }
}

  # create unique ID and add inverse psi
  unique_ids <- paste(data[{{grp}}], data[{{subgrp}}], sep = "_") |> unique()
  data$inv.water.potential <- -1 / (data[[wp.index]])

  unique_ids <- unique(data$unique_id)

  output_est <- list() # list of estimates for each unique id

  d_names <- names(data)

  for (i in unique_ids) {
    leaf_estimate <- data[data$unique_id == i, ]

    # fw.index=water mass, wp.index= water potential
    swc_swm_est <- SaturatedWaterContent(leaf_estimate, fw.index = fw.index, wp.index = wp.index, dm.index = dm.index)

    leaf_estimate[, "saturated.water.mass"] <- swc_swm_est[[1]]

    leaf_estimate[, "saturated.water.content"] <- swc_swm_est[[2]]

    leaf_estimate[, c("relative.water.content", "relative.water.deficit")] <- RelativeWaterCD(leaf_estimate, fw.index = fw.index)

    if (n_pts == T) {
      pts.vec <- check_n_pts(leaf_estimate, wp.index = "inv.water.potential", wm.index = "relative.water.deficit", method = method)

      if (is.character(pts.vec)) { # sometimes it is the case that none of the estimated cv values are < 10%; if this is the case set as 4

        pts <- 4
      } else {
        pts <- pts.vec[1]
      }
    } else {
      pts <- 4
    }

    ## need to change the amount of points included for estimation after TLP is estimated.
    tlp_est <- OsmoticEstimates(data = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row = pts) %>%
      EstimateTLP(., wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row_below = pts) %>%
      dplyr::pull(leaf.waterpotential.attlp) %>%
      unique()

    ## n rows above and below this tlp estimate
    row_above_tlp <- leaf_estimate %>%
      dplyr::filter(.data[[d_names[wp.index]]] > tlp_est) %>%
      nrow()

    row_below_tlp <- leaf_estimate %>%
      dplyr::filter(.data[[d_names[wp.index]]] < tlp_est) %>%
      nrow()

    if (row_above_tlp | row_below_tlp > nrow(data) - 4) {
      row_below_tlp <- 4

      row_above_tlp <- nrow(data) - row_below_tlp
    } else {

    }
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


col_quos <- function(...){
  
  args <- list2(...)
  cli::cli_alert_info(c("Grouping data by {as_name(grp)}"))
  if(sbgrp %in% args){
  cli::cli_alert_info(c("Subgroup of data: {as_name(sbgrp)}"))
  }
}
