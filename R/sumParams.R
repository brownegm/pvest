#' Summarize estimated pressure volume curve parameters by group 
#'
#' @param df A data frame. Data frame containing predicted parameters. 
#' @param group_name A string. Name of variable to group summary by.
#'
#' @return Returns a summarized data frame with the mean parameter values. 
#' @export
#' 
#' @import dplyr

sumParams<-function(df,group_name){ 
  
  .df<-df%>% 
    dplyr::group_by({{group_name}})%>%
    dplyr::select(!c(leaf,dry.weight,water.potential, fresh.weight, unique_id, inv.water.potential,
              relative.water.content, relative.water.deficit, osmotic.potential, pressure.potential, sym.rwc, sym.rwd))%>%
    dplyr::summarize(across(where(is.numeric), mean))
  
  return(.df)
  
  }