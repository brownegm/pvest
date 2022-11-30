#' Summarize estimated pressure volume curve parameters by group 
#'
#' @param df A data frame. Data frame containing predicted parameters. 
#' @param group_name A string. Name of variable to group summary by.
#' @param remove.cols A list. List of column names to be removed from using select. Default is leaf and unique_id
#' @return Returns a summarized data frame with the mean parameter values. 
#' 
#' @export
#' 
#' @import dplyr 
#' @importFrom tidyselect where

sumParams<-function(df,group_name, remove.cols=c("leaf", "unique_id")){ 
  
  group_summary<-df%>% 
    dplyr::group_by({{group_name}})%>%
    dplyr::select(!{{remove.cols}})%>%
    dplyr::summarize(across(where(is.numeric), mean))
  
  return(group_summary)
  
  }