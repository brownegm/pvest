###sensitivity of the saturated water content estimation

library(devtools)
load_all()

quag<-as.data.frame(pvest::data)
#create unique ID and add inverse psi
quag$unique_id<-paste(quag$species, quag$leaf, sep="_")
quag$inv.water.potential<--(1/(quag[,"water.potential"]))

unique_ids<-unique(quag$unique_id)

#####
cv<-function(x){sd(x, na.rm = T)/mean(x, na.rm=T)}
swc<-list()
cv_swc<-0.05
rows=2
ests<-data.frame(id=numeric())



swc[1]<-SaturatedWaterContent(quag, fw.index = 5, wp.index = 4, n_row=1)

while(cv_swc<0.05){ 

  if (rows>2){
  
  swc[rows]<-SaturatedWaterContent(quag, fw.index = 5, wp.index = 4, n_row=rows)
  cv_swc<-cv(c(swc[[rows]], swc[[rows-1]]))
  ests[rows,]<-c(swc[rows], cv_swc)

  }else{
  
    print("check")
  
  }
  
  rows=rows+1
}
output<-list()
ids<-unique_ids[1]


for(ids in unique_ids){

  ests[ids,1]<-ids
  
  for(i in 1:8){
    
  swc[i]<-SaturatedWaterContent(quag[quag$unique_id==ids, ], fw.index = 5, wp.index = 4, n_row=i)
  output<-swc
  ests[ids,2:9]<- output
  
  }
  
}
print(ests)

ests_osm<-data.frame(matrix(NA, nrow=8, ncol=4))
af<-NA
for(ids in unique_ids){
  
  ests_osm[,1]<-rep(ids, 8)
  
  for(i in 1:8){ 
leaf_estimate<-quag[quag$unique_id==ids,]
  
leaf_estimate[,"saturated.water.content"]<-SaturatedWaterContent(leaf_estimate, fw.index = 5, wp.index = 4)

leaf_estimate[,c("relative.water.content","relative.water.deficit")]<-RelativeWaterCD(leaf_estimate, fw.index=5)

leaf_estimate<-OsmoticEstimates(data=leaf_estimate, wc.index = "relative.water.deficit",wp.index = "inv.water.potential", n_row = i )

pi[i]<-unique(leaf_estimate$osm.pot.fullturgor)
af[i]<-unique(leaf_estimate$apoplastic.fraction)

ests_osm[ests_osm[,1]==ids,2]<- i
ests_osm[ests_osm[,1]==ids,3]<- pi
ests_osm[ests_osm[,1]==ids,4]<- af
  }
}
?OsmoticEstimates
