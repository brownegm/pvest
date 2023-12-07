
  
#roco1<-pv_dat%>%filter(species=="roco" & leaf=="1")

data<-pv_dat%>%filter(species=="roco"& leaf=="4")

#data<-pv_dat%>%filter(species=="alma"& leaf=="2")

fw.index = 5; wp.index = 4; dm.index = 3; n_pts = F
fw.index="fresh.weight"; wp.index= "water.potential"; 
#wp.index="inv.water.potential"; wm.index="relative.water.deficit"; 

# getting leaf estimate ---------------------------------------------------

# create unique ID and add inverse psi
data$unique_id <- paste(data$species, data$leaf, sep = "_")
data$inv.water.potential <- -1 / (data[[wp.index]])

unique_ids <- unique(data$unique_id)

output_est <- list() # list of estimates for each unique id

d_names <- names(data)

i<-unique_ids[1]

leaf_estimate <- data[data$unique_id == i, ]
  

swc_swm_est <- SaturatedWaterContent(leaf_estimate, fw.index = fw.index, wp.index = wp.index, dm.index = dm.index)
  
  leaf_estimate[, "saturated.water.mass"] <- swc_swm_est[[1]]
  
  leaf_estimate[, "saturated.water.content"] <- swc_swm_est[[2]]
  
  leaf_estimate[, c("relative.water.content", "relative.water.deficit")] <- RelativeWaterCD(leaf_estimate, fw.index = fw.index)
  



# other -------------------------------------------------------------------


# ROCO 1 & 4 , ALMA 2 problems as of 20230719


plot(leaf_estimate$relative.water.deficit, leaf_estimate$inv.water.potential)
leaf_estimate

t<-check_n_pts(pv_params_r2, wp.index="inv.water.potential", wm.index="relative.water.deficit", method = "all")#$cv10


pv_params_fil<-pv_params%>%filter(is.na(modulus))

plot

kl