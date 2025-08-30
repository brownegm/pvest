#' Estimate all pressure volume curve parameters for all leaves
#'
#' @param data A data frame. Input data frame with the raw values
#' @param group An unquoted character. The column name of the grouping variable
#' @param subgrp An unquoted character. The column name of the subgrouping variable
#' @param fw A numeric. Data frame index for the water content information.
#' @param wp A numeric. Indicates the index of the data frame including the water potential data
#' @param dm Numeric index of dry mass data.
#' @param method Specifies which method to pick number of rows below TLP. Options include "r2", "rmse", "aicc". See `?optim_thres()` for more information. If NULL, defaults to 4 rows.
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
#' @importFrom cli cli_alert_info cli_abort
#' @export
#' @rdname estPV

estPV <- function(data,
                  group,
                  subgrp = NULL,
                  fw,
                  wp,
                  dm,
                  method = NULL) {

  UseMethod("estPV")
}

#' @export
#' @rdname estPV
estPV.default <- function(data,
                          group,
                          subgrp = NULL,
                          fw,
                          wp,
                          dm,
                          method = NULL) {
  
  # Convert inputs to quosures for tidy evaluation
  grp <- rlang::enquo(group)
  fw <- rlang::enquo(fw)
  wp <- rlang::enquo(wp)
  dm <- rlang::enquo(dm)
  sbgrp <- rlang::enquo(subgrp)

  # Check that columns exist
  input_cols <- c(
    rlang::as_name(grp),
    rlang::as_name(fw),
    rlang::as_name(wp),
    rlang::as_name(dm)
  )

  missing_cols <- setdiff(input_cols, names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort("The following columns are missing from the data frame: {missing_cols}")
  }
  
  # if(!is.null(method) && !method %in% c("rmse", "aicc", "r2")) {
  #   cli::cli_abort("Invalid method specified. Choose from 'rmse', 'aicc', or 'r2'.")
  # }
  
  # Get subgrouping
  if (!rlang::quo_is_null(sbgrp)) {
    sbgrp_name <- rlang::as_name(sbgrp)

    if (!(sbgrp_name %in% names(data))) {
    cli::cli_abort("Specified subgroup ({rlang::as_name(sbgrp)}) does not exist in the data frame.")
    }
    cli::cli_alert_info("Grouping data by: {rlang::as_name(grp)} and {rlang::as_name(sbgrp)}")
  } else {
    cli::cli_alert_info("Grouping data by: {rlang::as_name(grp)}")
  }
  # create data list by unique grp and sbgrp combinations
  raw_data_list_by_sp <- by_grp_sbgrp(data, grp, sbgrp)

  # list of estimates for each unique id
  est <- vector(mode = "list", length = length(raw_data_list_by_sp))
  
  for (id in seq_along(raw_data_list_by_sp)) {
    if (!is.null(method)) {
      
      opt <- apply_optim(raw_data_list_by_sp[[id]],
                                      input_cols = input_cols,
                                      method = method)
      
      rows_above_below <- opt[[1]]
    }else{
      above_count <- nrow(raw_data_list_by_sp[[id]])-3
      rows_above_below <- c(above_count, 4)# the above should include the value below tlp too
      }

    rwc <- pvest::estRWC(
      raw_data_list_by_sp[[id]],
      fw.index = as_name(fw),
      wp.index = as_name(wp),
      dm.index = as_name(dm),
      n_row = rows_above_below[1], silent = T
    ) |>
      pvest::estOsmotic(
        data = _,
        n_row = rows_above_below[2],
        silent = T
      )
    tlp <- pvest::estTLP(
      data = rwc,
      n_row_above = rows_above_below[1]
    )
    est[[id]] <- do.call(cbind,
                                             list(
                                               rwc$data,
                                               tlp)
                                             )
    attr(est[[id]], "breakpoint") <- rows_above_below
    names(est)[id] <- raw_data_list_by_sp[[id]]["ids"] |> unique()
  }
  # combine all leaf estimates into one data frame.
  output_est <- structure(est,
                        creation_time = Sys.time(),
                        units = c(NA, NA, NA,
                                  "g","MPa","g", "g/g", "%",
                                  "%","%", "-MPa^-1",
                                  "MPa", "MPa","MPa",
                                  "MPa" ,"%", "%",
                                  "%","MPa", "%","%",
                                  "%","%","MPa", 
                                  "MPa","MPa^-1","MPa^-1",
                                  "MPa^-1","MPa^-1"),
                        class = c("estPV", "list")
                        )
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

#' Separate input dataframe by group and subgroup
#' @param x A dataframe
#' @param grp Grouping variable
#' @param sbgrp Subgrouping variable
by_grp_sbgrp <- function(x, grp, sbgrp = NULL) {
  data_with_unique_id <- x |>
    tidyr::unite("ids", !!grp, !!sbgrp, sep = "_", remove = FALSE)

  unique_ids <- dplyr::distinct(data_with_unique_id, .data$ids) |>
    dplyr::pull(.data$ids)

  obj <- lapply(unique_ids, \(id) filter(data_with_unique_id, .data$ids == id))

  invisible(obj)
}

#' @param x Object of classg _estPV_
#' @param ... Additional parameters passed to method
#' @rdname estPV
#' @export
#' 
print.estPV <- function(x, ...) {
  estPV_obj <- x
  units <- attr(estPV_obj, "units")[17:28]
  
  for (i in seq_along(estPV_obj)) {
    est <- estPV_obj[[i]]
    
    cat("Estimated PV parameters:\n")
    cat("----------------------------------------------------\n")
    cat("Species: ", unique(est$species), "\n")
    cat("Leaf:  ", unique(est$leaf), "\n")
    cat("----------------------------------------------------\n")
    cat("Osmotic potential at TLP:  ",
        unique(est$pi_tlp) |> round(3),
        units[1],
        "\n")
    cat("Bulk RWC at TLP:  ", unique(est$rwc_tlp) |> round(3), units[2], "\n")
    cat("Bulk RWD at TLP:  ", unique(est$rwd_tlp) |> round(3), units[3], "\n")
    cat("Symplastic RWC at TLP:  ",
        unique(est$sym_rwc_tlp) |> round(3),
        units[4],
        "\n")
    cat("Symplastic RWD at TLP:  ",
        unique(est$sym_rwd_tlp) |> round(3),
        units[5],
        "\n")
    cat("Bulk modulus at TLP:  ", unique(est$modulus) |> round(3), units[6], "\n")
    cat("Symplastic modulus at TLP:  ",
        unique(est$sym_modulus) |> round(3),
        units[7],
        "\n")
    cat("Bulk capacitance at FT:  ",
        unique(est$cap_bulk_ft )|> round(3),
        units[8],
        "\n")
    cat("Symplastic capacitance at FT: ",
        unique(est$cap_sym_ft) |> round(3),
        units[9],
        "\n")
    cat("Bulk Capacitance at TLP:  ",
        unique(est$cap_bulk_tlp) |> round(3),
        units[10],
        "\n")
    cat("Symplastic capacitance at TLP:  ",
        unique(est$cap_sym_tlp) |> round(3),
        units[11],
        "\n")
    cat("----------------------------------------------------\n")
    cat("-----------                                ---------\n")
    cat("----------------------------------------------------\n")
  }
}