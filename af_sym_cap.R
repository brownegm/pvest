#' Estimate the capacitance at full turgor and turgor loss point
#'
#' @param data A data frame
#' @param ft_tlp A string. "ft" indicates capacitance at full turgor and "tlp" 
#'     indicates leaf capacitance at turgor loss point
#'
#' @return Returns data frame 
#' @export 

capacitance_fttlp<-function(data, ft_tlp){
  
  cap.bulk=sma_slope(x=data$relative.water.content, y=data$water.potential)
  cap.sym=sma_slope(x=data$sym.rwc, y=data$water.potential)

  if(ft_tlp="ft"){
   
     data$cap.ft.bulk<-cap.bulk
     data$cap.ft.sym<-cap.sym
    
  }else{
    
    data$cap.tlp.bulk<-cap.bulk
    data$cap.tlp.sym<-cap.sym
    
  }
}
