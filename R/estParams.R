
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
  
  #stopifnot()#add check if the data frame is a dataframe
  #create unique ID and add inverse psi
  data$unique_id<-paste(data$species, data$leaf, sep="_")
  data$inv.water.potential<--(1/(data[,wp.index]))
  
  unique_ids<-unique(data$unique_id)
  
  output_est<-list()#list of estimates for each unique id
  
  for(i in unique_ids){ 
    
    leaf_estimate<-data[data$unique_id==i,]
    
    #fw.index=water mass, wp.index= water potential 
    leaf_estimate[,"saturated.water.content"]<-SaturatedWaterContent(leaf_estimate, fw.index = fw.index, wp.index = wp.index)
    
    leaf_estimate[,c("relative.water.content","relative.water.deficit")]<-RelativeWaterCD(leaf_estimate, fw.index=fw.index)
    
    leaf_estimate<-OsmoticEstimates(data=leaf_estimate, wc.index = "relative.water.deficit",wp.index = "inv.water.potential")
    
    leaf_estimate<-EstimateTLP(df=leaf_estimate, wc.index = "relative.water.deficit",wp.index = "inv.water.potential")
    
    #estimate other parameters:capacitance above and below tlp. 
    
    leaf_estimate_cap<-capacitance_fttlp(df=leaf_estimate, wc.index = "relative.water.content", s_wc.index = "sym.rwc", wp.index = "water.potential")
    
    #check this but should maybe work
    leaf_estimate[,"cap.ft.bulk"]<-rep(leaf_estimate_cap[1], nrow(leaf_estimate))
    leaf_estimate[,"cap.ft.sym"]<-rep(leaf_estimate_cap[2], nrow(leaf_estimate))
    leaf_estimate[,"cap.tlp.bulk"]<-rep(leaf_estimate_cap[3], nrow(leaf_estimate))
    leaf_estimate[,"cap.tlp.sym"]<-rep(leaf_estimate_cap[4], nrow(leaf_estimate))
    
    output_est[[i]]<-leaf_estimate
    
  }
  #combine all leaf estimates into one data frame.
  output_df<-as.data.frame(purrr::reduce(output_est, rbind))
  
  return(output_df)
}