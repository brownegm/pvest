#' Estimate the capacitance at full turgor and turgor loss point for the symplast and total. 
#'
#' @param data A data frame
#' @param wp.index Water potential index
#' @param wc.index Water content index. Here the expected value is total relative water content
#' @param s_wc.index Symplastic water content index. Here symplastic relative water content is expected
#' 
#' @return Returns vectors of estimated capacitance above and below turgor loss point 
#'     and for bulk tissue values and symplastic quantities.
#' @import dplyr 
#' @export 

capacitance_fttlp<-function(df, wp.index, wc.index, s_wc.index){
  
 #extract values above and below turgor loss point 
  data_abovetlp<-df%>% 
    dplyr::arrange(desc({{wp.index}}))%>%
    dplyr::filter(water.potential>leaf.waterpotential.attlp)%>%as.data.frame()
  
  data_belowtlp<-df%>% 
    dplyr::arrange(desc({{wp.index}}))%>%
    dplyr::filter(water.potential<leaf.waterpotential.attlp)%>%as.data.frame()
  
  #estimate capacitance 
  cap.ft.bulk=(sma_slope(x=data_abovetlp[,wc.index], y=data_abovetlp[,wp.index]))/100
  cap.ft.sym=(sma_slope(x=data_abovetlp[,s_wc.index], y=data_abovetlp[,wp.index]))/100
  cap.tlp.bulk=(sma_slope(x=data_belowtlp[,wc.index], y=data_belowtlp[,wp.index]))/100
  cap.tlp.sym=(sma_slope(x=data_belowtlp[,s_wc.index], y=data_belowtlp[,wp.index]))/100
  
  output<-c(cap.ft.bulk, cap.ft.sym, cap.tlp.bulk, cap.tlp.sym)
  
  return(output)
  
}
