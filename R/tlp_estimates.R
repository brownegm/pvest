#' Estimate parameters at turgor loss
#' @description Estimate leaf water potential, relative water content at turgor loss point and modulus of elasticity
#'    both bulk parameters and their symplastic counterparts
#'
#' @param data A data frame
#' @param fw.index Water associated variable
#' @param wp.index Pressure associated variable
#'
#' @return Returns data with new columns for the estimated parameters. 
#' @export

EstimateTlp<-function(data, fw.index, wp.index){

  slope=-sma_slope(x=data$pressure.potential, y=data$relative.water.deficit)
  intercept=sma_intercept(x=data$pressure.potential, y=data$relative.water.deficit, slope=slope)
  
  slope_sym=-sma_slope(x=data$pressure.potential, y=data$sym.rwc)
  intercept_sym=sma_intercept(x=data$pressure.potential, y=data$sym.rwc, slope=slope)
  
  pi.o_list<-OsmoticPotFullTurgor(data, fw.index, wp.index)[1:2]
     
    data$relative.water.deficit.attlp=-((intercept)/(slope))
    data$relative.water.content.attlp=100-data$relative.water.deficit.attlp
    data$sym.rwd.attlp<--((intercept_sym)/(slope_sym))
    data$leaf.waterpotential.attlp=-1/(pi.o_list[1]*data$relative.water.deficit.attlp + pi.o_list[2])
    data$modulus=max.pressure.potential/((100-data$relative.water.content.attlp)/100)
    data$modulus_sym=max.pressure.potential/((100-data$sym.rwd.attlp)/100)

}