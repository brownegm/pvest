#create function where it takes the slope and intercept functions and estimates parameters

# how to split above and below tlp? 
#option 1: functional above and below

# option 2: separate function which separates the data into two. input separator maybe a number for the error index or nrow 

# opt3: tbd

#the function saves the slopes and intercepts then estimates the above and below section. 



#slope and intercept functions shouldn't be output 

pp->data
estParams<-function(data, fw.index, wp.index){
  
  #data<-as.data.frame(data)
  
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
  
  for(i in unique_ids){
    
    leaf_estimate<-OsmoticEstimates(data[data$unique_id==i,], fw.index = "relative.water.deficit",wp.index = "inv.water.potential")
  
    data[i,]<-leaf_estimate
    }
  
  
}