##Test the package functionality with data from ITV-PV

# Create space ------------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(readr)

# load data ---------------------------------------------------------------

## individual mass and pv data
pv_dat<-read_excel(here("inst","extdata", "pvldcurvedata_10042022.xlsx"))%>%
  select(!c(fresh.weight.saturated, water.potential.bar, bag.weight,sp.leaf))%>%as.data.frame()

pvsps<-pv_dat%>%transmute(sp_indiv=paste0(toupper(species),leaf))

## paper data summarized
itvpv <- read_csv("inst/extdata/pvdat.csv")

itvpv<-itvpv%>%
  filter(!grepl(c("PLRA|ENFA|BAGA|QUAG|RAIN|CEBE"), spcode))#filter these out for the moment they add complexity

itvpv[itvpv$spcode=="CLLA","individual"]<-as.character(c(1:6))
itvpv[itvpv$spcode=="CLLA","spcind"]<-paste0("CLLA", c(1:6))

# match IDs of est output
itvpv<-itvpv%>%
  mutate(unique_id=paste(tolower(spcode),individual,sep = "_"), .before=swc)

#only the unique ids (SPECIES{1:X}) for the data 
unique.ids<-pvsps%>%filter(pvsps$sp_indiv%in%itvpv$spcind)%>%pull(1)%>%unique()

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
param_sum<-sumParams(pv_params, species)%>%select(species, saturated.water.content:cap.tlp.sym)
itvpv_sum<-sumParams(itvpv, spcode)

# combine measured and estimated ------------------------------------------
com<-left_join(itvpv, pv_params_byleaf, by="unique_id", suffix = c("", "_est"))
#figure this out!!!
#com_sp<-left_join(itvpv_sum, param_sum, by=species==spcode, suffix = c("", "_est"))

# plot things -------------------------------------------------------------
pdf(file=here::here("inst/extdata", "pv_params_leaf.pdf"))

plot(com$swc,com$saturated.water.content)

plot(com$pi_o,com$osm.pot.fullturgor)

plot(com$psi_tlp,com$leaf.waterpotential.attlp)

plot(com$af,com$apoplastic.fraction)

plot(com$rwc_tlp,com$relative.water.content.attlp)

plot(com$modulus,com$modulus_est)

plot(com$mod_sym,com$modulus_sym)

plot(com$cap_ft,com$cap.ft.bulk)

plot(com$cap_tlp,com$cap.tlp.bulk)

dev.off()

# output results ----------------------------------------------------------

write.csv(pv_leaf_uniques, here("inst/extdata/pv_uniques.csv"), row.names=F)

write.csv(pv_params_byleaf,here('inst/extdata', 'leafparams_sum.csv'), row.names = F)

write.csv(param_sum, here("inst/extdata", "sum_params.csv"))

write.csv(pv_params, here("inst/extdata", "pvparams.csv"))