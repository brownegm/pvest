##Test the package functionality with data from ITV-PV

# Create space ------------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)

# load data ---------------------------------------------------------------

#load("~/Documents/Research_UCLA/ITV-PV/data/itvdat_2022-07-16_.RData")
pv_dat<-read_excel(here("inst","extdata", "pvldcurvedata_10042022.xlsx"))%>%
  select(!c(fresh.weight.saturated, water.potential.bar, bag.weight,sp.leaf))%>%as.data.frame()

# compute parameter estimates ---------------------------------------------

pv_params<-estParams(pv_dat, fw.index = 5, wp.index = 4, dm.index = 3)

# summarize output --------------------------------------------------------

# summarize by leaf
pv_params_byleaf<-pv_params%>%  
  select(species, leaf, unique_id, saturated.water.content, osm.pot.fullturgor,apoplastic.fraction, 
                                       relative.water.deficit.attlp:cap.tlp.sym)%>%
  group_by(unique_id)%>%summarize_if(is.numeric, unique)

uniques<-read_excel(here("inst","extdata","summary_main.xlsx"), sheet = "uniques")%>%pull(unique_id)

pv_leaf_uniques<-pv_params_byleaf%>%
  filter(unique_id%in%uniques)

#summarize by species
param_sum<-sumParams(pv_params, species)

# output results ----------------------------------------------------------

write.csv(pv_leaf_uniques, here("inst/extdata/pv_uniques.csv"), row.names=F)

write.csv(pv_params_byleaf,here('inst/extdata', 'leafparams_sum.csv'), row.names = F)

write.csv(param_sum, here("inst/extdata", "sum_params.csv"))

write.csv(pv_params, here("inst/extdata", "pvparams.csv"))