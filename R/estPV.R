#' Estimate all pressure volume curve parameters for all leaves
#'
#' @param data A data frame. Input data frame with the raw values
#' @param fw A numeric. Data frame index for the water content information.
#' @param wp A numeric. Indicates the index of the data frame including the water potential data
#' @param dm Numeric index of dry mass data.
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
#' @import dplyr
#' @importFrom rlang enquo as_name
#' @import cli
#' @export
#' @rdname estPV

estPV <- function(data,
                  group,
                  subgrp = NULL,
                  fw,
                  wp,
                  dm,
                  n_pts,
                  method = NULL,
                  ...) {
  if (n_pts == T & is.null(method)) {
    "Method must be specified when n_pts=T"
  }
  UseMethod("estPV")
}

#'@export
#'@rdname estPV
estPV.default <- function(data,
                          group,
                          subgrp = NULL,
                          fw,
                          wp,
                          dm,
                          n_pts = F,
                          method = NULL) {
  # Convert inputs to quosures for tidy evaluation
  grp <- rlang::enquo(group)
  fw <- rlang::enquo(fw)
  wp <- rlang::enquo(wp)
  dm <- rlang::enquo(dm)
  sbgrp <- rlang::enquo(subgrp)
  
  # Check that columns exist
  input_cols <- c(rlang::as_name(grp),
                  rlang::as_name(fw),
                  rlang::as_name(wp),
                  rlang::as_name(dm))
  
  missing_cols <- setdiff(input_cols, names(data))
  
  if (length(missing_cols) > 0) {
    cli::cli_abort("The following columns are missing from the data frame: {missing_cols}")
  }
  # Get optional subgrouping
  if (!rlang::quo_is_null(sbgrp)) {
    sbgrp_name <- rlang::as_name(sbgrp)
    
    if (!(sbgrp_name %in% names(data))) {
      cli::cli_abort("Specified subgroup ({rlang::as_name(sbgrp)}) does not exist in the data frame.")
    }
    cli::cli_alert_info("Grouping data by: {rlang::as_name(grp)} and {rlang::as_name(sbgrp)}")
  } else{
    cli::cli_alert_info("Grouping data by: {rlang::as_name(grp)}")
  }
  # create data list by unique grp and sbgrp combinations
  raw_data_list_by_sp <- by_grp_sbgrp(data, grp, sbgrp)

  n_row_below <- 4
  # list of estimates for each unique id
  output_est <- vector(mode = "list", length = length(raw_data_list_by_sp))

  for (id in seq_along(raw_data_list_by_sp)) {
    
    output_est [[id]] <- pvest::estRWC(
      raw_data_list_by_sp[[id]],
      fw.index = as_name(fw),
      wp.index = as_name(wp),
      dm.index = as_name(dm),
      n_row = 4, silent = T)|>
      pvest::estOsmotic(obj=_,##UPDATE 
                          n_row = n_row_below,
                          silent = T)|>
      pvest::estTLP(osm_obj=_,
                     n_row_above = 5) ##UPDATE

    names(output_est)[id] <- raw_data_list_by_sp[[id]]["ids"]|>unique()
  }
  # combine all leaf estimates into one data frame.
  # output_df <- as.data.frame(purrr::reduce(output_est, rbind))
  invisible(output_est)
}
#     if (n_pts == T) {
#       pts.vec <- check_n_pts(leaf_estimate, wp.index = "inv.water.potential", wm.index = "relative.water.deficit", method = method)
#
#       if (is.character(pts.vec)) { # sometimes it is the case that none of the estimated cv values are < 10%; if this is the case set as 4
#
#         pts <- 4
#       } else {
#         pts <- pts.vec[1]
#       }
#     } else {
#       pts <- 4
#     }
#
#     ## need to change the amount of points included for estimation after TLP is estimated.
#     tlp_est <- OsmoticEstimates(data = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row = pts) %>%
#       EstimateTLP(., wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row_below = pts) %>%
#       dplyr::pull(leaf.waterpotential.attlp) %>%
#       unique()
#
#     ## n rows above and below this tlp estimate
#     row_above_tlp <- leaf_estimate %>%
#       dplyr::filter(.data[[d_names[wp.index]]] > tlp_est) %>%
#       nrow()
#
#     row_below_tlp <- leaf_estimate %>%
#       dplyr::filter(.data[[d_names[wp.index]]] < tlp_est) %>%
#       nrow()
#
#     if (row_above_tlp | row_below_tlp > nrow(data) - 4) {
#       row_below_tlp <- 4
#
#       row_above_tlp <- nrow(data) - row_below_tlp
#     } else {
#
#     }
#     leaf_estimate <- OsmoticEstimates(data = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row = row_below_tlp) # osmotic variables are estimated based on inv.psi vs RWD below TLP
#
#     leaf_estimate <- EstimateTLP(df = leaf_estimate, wc.index = "relative.water.deficit", wp.index = "inv.water.potential", n_row_above = row_above_tlp, n_row_below = row_below_tlp

# col_quos <- function(...){
#
#   args <- list2(...)
#   cli::cli_alert_info(c("Grouping data by {as_name(grp)}"))
#   if(sbgrp %in% args){
#   cli::cli_alert_info(c("Subgroup of data: {as_name(sbgrp)}"))
#   }
# }

#' by_grp_sbgrp
#' @param x A dataframe
#' @param grp Grouping variable
#' @param sbgrp Subgrouping variable
by_grp_sbgrp <- function(x, grp, sbgrp = NULL) {
  data_with_unique_id <- x |>
    tidyr::unite("ids", !!grp, !!sbgrp, sep = "_", remove = FALSE)
  
  unique_ids <- dplyr::distinct(data_with_unique_id, ids) |>
    dplyr::pull(ids)
  
  obj <- lapply(unique_ids, \(id) filter(data_with_unique_id, ids == id))
  
  invisible(obj)
}
