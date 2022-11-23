#' Summarize estimated pressure volume curve parameters by group 
#'
#' @param df A data frame. Data frame containing predicted parameters. 
#' @param group_name A string. Name of variable to group summary by.
#'
#' @return Returns a summarized data frame with the mean parameter values. 
#' 
#' @export
#' 
#' @import dplyr

sumParams<-function(df,group_name){ 
  
  group_summary<-df%>% 
    dplyr::group_by({{group_name}})%>%
    dplyr::select(!c(leaf, unique_id))%>%
    dplyr::summarize(across(where(is.numeric), mean))
  
  return(group_summary)
  
  }