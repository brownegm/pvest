##Test the package functionality with data from ITV-PV

# Create space ------------------------------------------------------------

library(tidyverse)
library(here)
library(pvest)

# load data ---------------------------------------------------------------

## individual mass and pv data
pv_dat<-read_excel(here("inst","extdata", "pvldcurvedata_10042022.xlsx"))%>%
  select(!c(fresh.weight.saturated, water.potential.bar, bag.weight,sp.leaf))%>%as.data.frame()

pvsps<-pv_dat%>%transmute(sp_indiv=paste0(toupper(species),leaf))

#only the unique ids (SPECIES{1:X}) for the data 
unique.ids<-pvsps%>%dplyr::filter(pvsps$sp_indiv%in%itvpv$spcind)%>%pull(1)%>%unique()

## paper data summarized
itvpv <- readr::read_csv(here("inst/extdata/pvdat.csv"))

itvpv<-itvpv%>%
  filter(!grepl(c("PLRA|ENFA|BAGA|QUAG|RAIN|CEBE"), spcode))#filter these out for the moment they add complexity

itvpv[itvpv$spcode=="CLLA","individual"]<-as.character(c(1:6))
itvpv[itvpv$spcode=="CLLA","spcind"]<-paste0("CLLA", c(1:6))

# match IDs of est output
itvpv<-itvpv%>%
  mutate(unique_id=paste(tolower(spcode),individual,sep = "_"), .before=swc)

itvpv_byspecies<-sumParams(itvpv, spcode)#summarize the manual estimates by species

# compute parameter estimates ---------------------------------------------

# filter the unique ids that aren't in the summarized df
pv_dat_fil<-pv_dat%>%
  filter(paste0(toupper(species),leaf) %in% unique.ids)

# compute
pv_params<-estParams(pv_dat_fil, fw.index = 5, wp.index = 4, dm.index = 3)

# summarize output --------------------------------------------------------

# summarize by leaf
pv_params_byleaf<-pv_params%>%  
  select(species, leaf, unique_id, saturated.water.content, osm.pot.fullturgor,apoplastic.fraction, 
                                       relative.water.deficit.attlp:cap.tlp.sym)%>%
  group_by(unique_id)%>%summarize_if(is.numeric, unique)

uniques<-read_excel(here("inst","extdata","summary_main.xlsx"), sheet = "unique_ids")%>%pull(unique_id)

pv_leaf_uniques<-pv_params_byleaf%>%
  filter(unique_id%in%uniques)

#summarize by species
pv_params_byspecies<-sumParams(pv_params, species)#%>%select(species, saturated.water.content:cap.tlp.sym)


# combine measured and estimated ------------------------------------------
com<-right_join(itvpv, pv_params_byleaf, by="unique_id", suffix = c("", "_est"))

com_species<-right_join(itvpv_byspecies, pv_params_byspecies, by="unique_id", suffix = c("", "_est"))

#figure this out!!!
#com_sp<-left_join(itvpv_sum, param_sum, by=species==spcode, suffix = c("", "_est"))

# Estimate OLS fits and prediction intervals (BY LEAF) ----------------------------------

f<-function(data, var){
  
  new_preds<-data%>%# create df with just the variable of interest 
    select(any_of(var))%>%
    transmute(var=seq(min(data[,var], na.rm = T),max(data[,var], na.rm = T), length.out=nrow(data)))
  
  names(new_preds)[1]<-{{var}} # rename that column to match
  
  return(new_preds)
}

# variables pred and manual
og_vars<-names(com)[9:20]
pred_vars<-names(com)[c(22:24, 26, 28:35)]
#reorder pred_vars to match og_vars
pred_vars<-pred_vars[c(1,2,6,3:5,7:8,9,11,10,12)]
pred_vars_index<-which(names(com)%in%pred_vars)[c(1,2,6,3:5,7:8,9,11,10,12)] # as index numbers

# estimate linear models and summaries
pv_lms<-purrr::map2(og_vars, pred_vars,~lm(com[[.x]]~com[[.y]])) #compute linear models for each variable pair

pv_lms.v2<-purrr::map2(c(9:20),pred_vars_index, 
                       ~lm(as.formula(paste(names(com)[.x], "~", names(com)[.y])), data=com)) #compute linear models for each variable pair

pv_lms_summary<-purrr::map(pv_lms,\(lm) summary(lm)) #save lm summaries 


pv_lms.origin<-purrr::map2(c(9:20),pred_vars_index, 
                       ~lm(as.formula(paste(names(com)[.x], "~", "0+", names(com)[.y])), data=com)) #compute linear models for each variable pair

#prep for prediction intervals
#see ?predict.lm
#predictions<-predict(test, interval="prediction")

# f(com, "saturated.water.content")

pv_<-map(pred_vars_index, \(coln) f(data=com, names(com)[coln]))# use function f to create "newdata" dataframes for each variable

# estimate prediction intervals
pv_pred_intervals <- map2(pv_lms.v2,
                          pv_,
                          ~ predict(.x, newdata = .y, interval = "prediction") %>%
                            as.data.frame)

pv_pred_intervals.origin <- map2(pv_lms.origin,
                                 pv_,
                                 ~ predict(.x, newdata = .y, interval = "prediction") %>%
                                    as.data.frame)


pv_pred_intervals.origin

find_outlier<-function(x,y, index){
  
  temp<-merge(x,y)
  
  var<-names(temp)[index]
  
  temp_output<-temp%>%
    filter({{index}}<=lwr|{{index}}>=upr)
  
  return(temp_output)
  
}

x=pv_pred_intervals[[1]]; y=pv_[[1]]
t<-find_outlier(x=pv_pred_intervals[[1]], y=pv_[[1]], 4)

test<-merge(pv_pred_intervals[[1]], pv_[[1]])

test.filt<-test%>% #these are the saturated water content values that are greater than or less than the pred.int
  filter(saturated.water.content<=lwr|saturated.water.content>=upr)

test<-cbind(pv_pred_intervals[[2]], pv_[[2]])

test.filt<-test%>% #these are the saturated water content values that are greater than or less than the pred.int
  filter(osm.pot.fullturgor<=lwr|osm.pot.fullturgor>=upr)
# new_preds<-com%>%transmute(saturated.water.content=seq(0,max(com$saturated.water.content), length.out=nrow(com)))
# 
# newx <- seq(0,max(com$saturated.water.content), length.out=nrow(newdata))
# 
# newdata <- data.frame(newx)
# 
# pred_params <- predict(pv_lms.v2[[1]], newdata=new_preds, interval="prediction", level=0.95)%>%as.data.frame()
# 
# a<-predict(pv_lms[[1]], interval="prediction", level = 0.95)

# plot things -------------------------------------------------------------
pdf(file=here::here("inst/extdata", "pv_params_leaf.pdf"))

plot(com$swc~com$saturated.water.content, 
     pch=21,cex=2,
     col="black",bg= "#4f8359")

abline(a=0, b=1, lwd=2)

abline(pv_lms.v2[[1]], col="#4f8359", lwd=3)

lines(pv_[[1]][[1]], pv_pred_intervals[[1]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[1]][[1]], pv_pred_intervals[[1]][[2]], col="orangered", lty="dashed", lwd=2)#lower

lines(pv_[[1]][[1]], pv_pred_intervals.origin[[1]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[1]][[1]], pv_pred_intervals.origin[[1]][[2]], col="orangered", lty="dashed", lwd=2)#lower

legend("bottomright", legend=c("1:1", "OLS", "Pred.Int"), lty=c(1,1,2), col=c("black", "#4f8359", "orangered"), lwd=2, bty="n")

plot(com$osm.pot.fullturgor, com$pi_o,
     pch=21,cex=2,
     col="black",bg= "#4f8359")

abline(a=0, b=1, lwd=2)

abline(pv_lms.v2[[2]], col="#4f8359", lwd=3)

abline(pv_lms.origin[[2]], col="#4f8359", lwd=3)

lines(pv_[[2]][[1]], pv_pred_intervals[[2]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[2]][[1]], pv_pred_intervals[[2]][[2]], col="orangered", lty="dashed", lwd=2)#lower

lines(pv_[[2]][[1]], pv_pred_intervals.origin[[2]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[2]][[1]], pv_pred_intervals.origin[[2]][[2]], col="orangered", lty="dashed", lwd=2)#lower

legend("bottomright", legend=c("1:1", "OLS", "Pred.Int"), lty=c(1,1,2), col=c("black", "#4f8359", "orangered"), lwd=2, bty="n")

plot(com$leaf.waterpotential.attlp, com$psi_tlp,
     pch=21,cex=2,
     col="black",bg= "#4f8359")

abline(a=0, b=1, lwd=2)

abline(pv_lms.v2[[3]], col="#4f8359", lwd=3)

lines(pv_[[3]][[1]], pv_pred_intervals[[3]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[3]][[1]], pv_pred_intervals[[3]][[2]], col="orangered", lty="dashed", lwd=2)#lower

lines(pv_[[3]][[1]], pv_pred_intervals.origin[[3]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[3]][[1]], pv_pred_intervals.origin[[3]][[2]], col="orangered", lty="dashed", lwd=2)#lower

legend("bottomright", legend=c("1:1", "OLS", "Pred.Int"), lty=c(1,1,2), col=c("black", "#4f8359", "orangered"), lwd=2, bty="n")

plot(com$apoplastic.fraction, com$af,
     pch=21,cex=2,
     col="black",bg= "#4f8359")

abline(a=0, b=1, lwd=2)

abline(pv_lms.v2[[4]], col="#4f8359", lwd=3)

lines(pv_[[4]][[1]], pv_pred_intervals[[4]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[4]][[1]], pv_pred_intervals[[4]][[2]], col="orangered", lty="dashed", lwd=2)#lower

lines(pv_[[4]][[1]], pv_pred_intervals.origin[[4]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[4]][[1]], pv_pred_intervals.origin[[4]][[2]], col="orangered", lty="dashed", lwd=2)#lower

legend("bottomright", legend=c("1:1", "OLS", "Pred.Int"), lty=c(1,1,2), col=c("black", "#4f8359", "orangered"), lwd=2, bty="n")

plot(com$relative.water.content.attlp, com$rwc_tlp,
     pch=21,cex=2,
     col="black",bg= "#4f8359")

abline(a=0, b=1, lwd=2)

abline(pv_lms.v2[[5]], col="#4f8359", lwd=3)

lines(pv_[[5]][[1]], pv_pred_intervals[[5]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[5]][[1]], pv_pred_intervals[[5]][[2]], col="orangered", lty="dashed", lwd=2)#lower

lines(pv_[[5]][[1]], pv_pred_intervals.origin[[5]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[5]][[1]], pv_pred_intervals.origin[[5]][[2]], col="orangered", lty="dashed", lwd=2)#lower

legend("bottomright", legend=c("1:1", "OLS", "Pred.Int"), lty=c(1,1,2), col=c("black", "#4f8359", "orangered"), lwd=2, bty="n")

plot(com$modulus_est, com$modulus,
     pch=21,cex=2,
     col="black",bg= "#4f8359")

abline(a=0, b=1, lwd=2)

abline(pv_lms.v2[[7]], col="#4f8359", lwd=3)

lines(pv_[[7]][[1]], pv_pred_intervals[[7]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[7]][[1]], pv_pred_intervals[[7]][[2]], col="orangered", lty="dashed", lwd=2)#lower

lines(pv_[[7]][[1]], pv_pred_intervals.origin[[7]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[7]][[1]], pv_pred_intervals.origin[[7]][[2]], col="orangered", lty="dashed", lwd=2)#lower

legend("bottomright", legend=c("1:1", "OLS", "Pred.Int"), lty=c(1,1,2), col=c("black", "#4f8359", "orangered"), lwd=2, bty="n")

plot(com$modulus_sym, com$mod_sym,
     pch=21,cex=2,
     col="black",bg= "#4f8359")

abline(a=0, b=1, lwd=2)

abline(pv_lms.v2[[8]], col="#4f8359", lwd=3)

lines(pv_[[8]][[1]], pv_pred_intervals[[8]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[8]][[1]], pv_pred_intervals[[8]][[2]], col="orangered", lty="dashed", lwd=2)#lower

lines(pv_[[8]][[1]], pv_pred_intervals.origin[[8]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[8]][[1]], pv_pred_intervals.origin[[8]][[2]], col="orangered", lty="dashed", lwd=2)#lower

legend("bottomright", legend=c("1:1", "OLS", "Pred.Int"), lty=c(1,1,2), col=c("black", "#4f8359", "orangered"), lwd=2, bty="n")

plot(com$cap.ft.bulk, com$cap_ft,
     pch=21,cex=2,
     col="black",bg= "#4f8359")

abline(a=0, b=1, lwd=2)

abline(pv_lms.v2[[9]], col="#4f8359", lwd=3)

lines(pv_[[9]][[1]], pv_pred_intervals[[9]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[9]][[1]], pv_pred_intervals[[9]][[2]], col="orangered", lty="dashed", lwd=2)#lower

lines(pv_[[9]][[1]], pv_pred_intervals.origin[[9]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[9]][[1]], pv_pred_intervals.origin[[9]][[2]], col="orangered", lty="dashed", lwd=2)#lower

legend("bottomright", legend=c("1:1", "OLS", "Pred.Int"), lty=c(1,1,2), col=c("black", "#4f8359", "orangered"), lwd=2, bty="n")

plot(com$cap.tlp.bulk, com$cap_tlp,
     pch=21,cex=2,
     col="black",bg= "#4f8359")

abline(a=0, b=1, lwd=2)

abline(pv_lms.v2[[10]], col="#4f8359", lwd=3)

lines(pv_[[10]][[1]], pv_pred_intervals[[10]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[10]][[1]], pv_pred_intervals[[10]][[2]], col="orangered", lty="dashed", lwd=2)#lower

lines(pv_[[10]][[1]], pv_pred_intervals.origin[[10]][[3]], col="orangered", lty="dashed", lwd=2)#upper
lines(pv_[[10]][[1]], pv_pred_intervals.origin[[10]][[2]], col="orangered", lty="dashed", lwd=2)#lower

legend("bottomright", legend=c("1:1", "OLS", "Pred.Int"), lty=c(1,1,2), col=c("black", "#4f8359", "orangered"), lwd=2, bty="n")

dev.off()


# output results ----------------------------------------------------------

write.csv(pv_leaf_uniques, here("inst/extdata/pv_uniques.csv"), row.names=F)

write.csv(pv_params_byleaf,here('inst/extdata', 'leafparams_sum.csv'), row.names = F)

write.csv(param_sum, here("inst/extdata", "sum_params.csv"))

write.csv(pv_params, here("inst/extdata", "pvparams.csv"))