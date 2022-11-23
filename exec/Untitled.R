##Test the package functionality with data from ITV-PV



# Create space ------------------------------------------------------------

library(tidyverse)
library(here)

#load_data
load("~/Documents/Research_UCLA/ITV-PV/data/itvdat_2022-07-16_.RData")
pv_dat<-read_excel(here("inst","pvldcurvedata_10042022.xlsx"))%>%
  select(!c(dry.weight, fresh.weight.saturated, water.potential.bar, bag.weight,sp.leaf))%>%as.data.frame()

#compute parameter estimates
pv_params<-estParams(pv_dat, fw.index = 4, wp.index = 3)

#summarize by species
param_sum<-sumParams(pv_params, species)


