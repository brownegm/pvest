


#' Estimate the RWC and RWD at turgor loss point for using the relationship between leaf pressure potential and
#'     bulk and symplastic relative water deficit
#'
#' @param data A data frame. Must contain pressure potential and relative water deficit values.
#' @param psip.index TBD 
#' @param rwd.index TBD
#' @param sym_rwd.index TBD
#'
#' @return Bulk and symplastic slopes and intercepts
#' @export
#'
PsiPRWD_slopeint <- function(data, psip.index, rwd.index, sym_rwd.index){

slope=-sma_slope(x=data[,psip.index], y=data[,rwd.index])
intercept=sma_intercept(x=data[,psip.index], y=data[,rwd.index], slope=slope)

slope_sym=-sma_slope(x=data[,psip.index], y=data[,sym_rwd.index])
intercept_sym=sma_intercept(x=data[,psip.index], y=data[,sym_rwd.index], slope=slope_sym)

out<-c(slope, intercept, slope_sym, intercept_sym)

return(out)

}

#' Estimate parameters at turgor loss
#' @description Estimate leaf water potential, relative water content at turgor loss point and modulus of elasticity
#'    both bulk parameters and their symplastic counterparts
#'
#' @param data A data frame
#' @param fw.index Water associated variable
#' @param wp.index Pressure associated variable
#'
#' @return Returns data with new columns for the estimated parameters. 
#' 
#' @import dplyr
#' @export

EstimateTLP<-function(data, fw.index, wp.index, n_row=4){

  data_belowtlp<-data%>% 
    dplyr::arrange(desc({{wp.index}}))%>%
    dplyr::slice_tail(n=n_row)%>%as.data.frame()
  
  data_abovetlp<-data%>% 
    dplyr::arrange(desc({{wp.index}}))%>%
    dplyr::slice_head(n=n_row)%>%as.data.frame()
  
  psip.rwd_list<-PsiPRWD_slopeint(data=data_abovetlp, 
                                  psip.index = "pressure.potential", 
                                  rwd.index="relative.water.deficit",
                                  sym_rwd.index = "sym.rwc") #output is a list in order: slope, intercept, sym slope, sym intercept
  
  pi.o_list<-OsmoticPotFullTurgor(data=data_belowtlp, fw.index, wp.index)[1:2]
     
    data$relative.water.deficit.attlp=-((psip.rwd_list[[2]])/(psip.rwd_list[[1]]))
    data$relative.water.content.attlp=100-data$relative.water.deficit.attlp
    data$sym.rwd.attlp<--((psip.rwd_list[[4]])/(psip.rwd_list[[3]]))
    data$leaf.waterpotential.attlp=-1/(pi.o_list[1]*data$relative.water.deficit.attlp + pi.o_list[2])
    data$modulus=data$max.psip/((100-data$relative.water.content.attlp)/100)
    data$modulus_sym=data$max.psip/((100-data$sym.rwd.attlp)/100)
    
    return(data)

}