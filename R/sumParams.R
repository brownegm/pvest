#' Summarize estimated pressure volume curve parameters by group
#'
#' @param df A data frame. Data frame containing predicted parameters.
#' @param group_name A string. Name of variable to group summary by.
#' @param remove.cols T/F. Should columns be removed in the output
#' @param cols.to.remove A list. List of column names to be removed from using select. Default is leaf and unique_id
#' @return Returns a summarized data frame with the mean parameter values.
#'
#' @export
#'
#' @import dplyr
#' @importFrom tidyselect vars_select_helpers

sumParams <- function(df, group_name, remove.cols = F, cols.to.remove = c("leaf", "unique_id")) {
  if (remove.cols == F) {
    group_summary <- df %>%
      dplyr::group_by({{ group_name }}) %>%
      dplyr::summarize(across(tidyselect::vars_select_helpers$where(is.numeric), mean(., na.rm = T)))
  } else {
    group_summary <- df %>%
      dplyr::group_by({{ group_name }}) %>%
      dplyr::select(!{{ cols.to.remove }}) %>%
      dplyr::summarize(across(tidyselect::vars_select_helpers$where(is.numeric), mean(., na.rm = T)))
  }
  return(group_summary)
}
