
#' Estimate all pressure volume curve parameters for all leaves
#' 
#' @param data A data frame. Input data frame with the raw values 
#' @param fw.index A numeric. Data frame index for the water content information. 
#' @param wp.index A numeric. Indicates the index of the data frame including the water potential data
#' 
#' @details 
#'     Data needs to have species or individual information as well as leaf identifiers. 
#'     The function takes your "water.potential" column(recommend naming it thus) and a "fresh water content"(again, ideally named fresh.water.content) for multiple 
#'     hydration states. For information on the production of leaf pressure volume curves see Sack 2011. 
#'     With the data for each leaf it takes the first for values and estimates a saturated water content, estimates
#'     relative water content and then with the relationship between relative water deficit and 1/psi, it estimates the 
#'     osmotic potential and pressure potential at full turgor(max.psip) from which the same parameters are estimated for each
#'     subsequent hydration state. 
#' 
#' @return Returns a data frame with the estimated parameters 
#' 
#' @importFrom purrr reduce
#' @import dplyr
#' @export
#'
estParams<-function(data, fw.index, wp.index){
  
  #create unique ID and add inverse psi
  data$unique_id<-paste(data$species, data$leaf, sep="_")
  data$inv.water.potential<--(1/(data$water.potential))
  
  unique_ids<-unique(data$unique_id)

  #loop over each leaf saturated water content and rwc
  for(i in unique_ids){ 
    
    #first select the first for values and estimate SWC and RWC values 
    data_abovetlp<-data[data$unique_id==i,]%>% 
      dplyr::arrange(desc(water.potential))%>%
      dplyr::slice_head(n=4)%>%as.data.frame()
    
    #fw.index=water mass, wp.index= water potential 
    data[data$unique_id==i,"saturated.water.content"]<-SaturatedWaterContent(data_abovetlp[data_abovetlp$unique_id==i,], fw.index = fw.index, wp.index = wp.index)
  
    data[,c("relative.water.content","relative.water.deficit")]<-RelativeWaterCD(data, fw.index=5)
  
  }
  
  temp<-list()
  
  for(i in unique_ids){
    
    leaf_estimate<-OsmoticEstimates(data[data$unique_id==i,], fw.index = "relative.water.deficit",wp.index = "inv.water.potential")
  
    temp[[i]]<-leaf_estimate
    
    }

  data_t<-as.data.frame(reduce(temp, rbind))
  
  for(i in unique_ids){
    
    leaf_estimate_tlps<-EstimateTLP(df=data_t[data_t$unique_id==i,], fw.index = "relative.water.deficit",wp.index = "inv.water.potential")
    
    temp[[i]]<-leaf_estimate_tlps
    
  }
 
  data_tt<-as.data.frame(reduce(temp, rbind))
  
  #estimate other parameters:capacitance above and below tlp. 
  
  for(i in unique_ids){
    
    leaf_estimate_cap<-capacitance_fttlp(df=data_tt[data_tt$unique_id==i,], wc.index = "relative.water.content", s_wc.index = "sym.rwc", wp.index = "water.potential")
    
    data_tt[data_tt$unique_id==i, "cap.ft.bulk"]<-rep(leaf_estimate_cap[1], nrow(data_tt[data_tt$unique_id==i,]))
    data_tt[data_tt$unique_id==i,"cap.ft.sym"]<-rep(leaf_estimate_cap[2], nrow(data_tt[data_tt$unique_id==i,]))
    data_tt[data_tt$unique_id==i, "cap.tlp.bulk"]<-rep(leaf_estimate_cap[3], nrow(data_tt[data_tt$unique_id==i,]))
    data_tt[data_tt$unique_id==i, 'cap.tlp.sym']<-rep(leaf_estimate_cap[4], nrow(data_tt[data_tt$unique_id==i,]))
  }
  
  return(data_tt)
}