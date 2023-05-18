##Test the package functionality with data from ITV-PV

# Create space ------------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(readr)
library(pvest)

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


# Estimate prediction fits and intervals ----------------------------------

og_vars<-names(com)[9:20]
pred_vars<-names(com)[c(22:24, 26, 28:35)]
#reorder pred_vars to match og_vars
pred_vars<-pred_vars[c(1,2,6,3:5,7:8,9,11,10,12)]


pv_lms<-purrr::map2(og_vars, pred_vars, \(og,pred) lm(as.formula(og~pred), data = com))

pv_lms<-mapply(og_vars, pred_vars, \(og,pred) lm(as.formula(og~pred), data = com))

model.ols.Dt <- lm(Dtobserved~Dtestimated-1, data=data.trichome.surface.factor)
summary(model.ols.Dt)

model.ols.Ds <- lm(Dsoberved~Dsestimated-1, data=data.trichome.surface.factor)
summary(model.ols.Ds)

newdata <- data.frame(data.trichome.surface.factor$Dtestimated)
newdataDs <- data.frame(data.trichome.surface.factor$Dsestimated)

newx <- seq(0, 450, by=1)
newy <- seq(0, 900, by=1)

Dtestimated <- data.trichome.surface.factor$Dtestimated
Dsestimated <- data.trichome.surface.factor$Dsestimated

pred_interval.Dt <- predict(model.ols.Dt, newdata=data.frame(Dtestimated=newx), interval="prediction", level=0.95)
pred_interval.Ds <- predict(model.ols.Ds, newdata=data.frame(Dsestimated=newy), interval="prediction", level=0.95)
model.ols.Dt <- lm(Dtobserved~Dtestimated-1, data=data.trichome.surface.factor)
summary(model.ols.Dt)


model.ols.Ds <- lm(Dsoberved~Dsestimated-1, data=data.trichome.surface.factor)
summary(model.ols.Ds)

newdata <- data.frame(data.trichome.surface.factor$Dtestimated)
newdataDs <- data.frame(data.trichome.surface.factor$Dsestimated)

newx <- seq(0, 450, by=1)
newy <- seq(0, 900, by=1)

Dtestimated <- data.trichome.surface.factor$Dtestimated
Dsestimated <- data.trichome.surface.factor$Dsestimated

pred_interval.Dt <- predict(model.ols.Dt, newdata=data.frame(Dtestimated=newx), interval="prediction", level=0.95)
pred_interval.Ds <- predict(model.ols.Ds, newdata=data.frame(Dsestimated=newy), interval="prediction", level=0.95)

# plot things -------------------------------------------------------------
pdf(file=here::here("inst/extdata", "pv_params_leaf.pdf"))

plot(com$swc,com$saturated.water.content, 
     pch=21, col="black",bg= "green")

abline(a=0, b=1, lwd=2)

plot(com$pi_o,com$osm.pot.fullturgor, 
     pch=21, col="black",bg= "green")

abline(a=0, b=1, lwd=2)

plot(com$psi_tlp,com$leaf.waterpotential.attlp, 
     pch=21, col="black",bg= "green")

abline(a=0, b=1, lwd=2)

plot(com$af,com$apoplastic.fraction, 
     pch=21, col="black",bg= "green")

abline(a=0, b=1, lwd=2)

plot(com$rwc_tlp,com$relative.water.content.attlp, 
     pch=21, col="black",bg= "green")

abline(a=0, b=1, lwd=2)

plot(com$modulus,com$modulus_est, 
     pch=21, col="black",bg= "green")

abline(a=0, b=1, lwd=2)

plot(com$mod_sym,com$modulus_sym, 
     pch=21, col="black",bg= "green")

abline(a=0, b=1, lwd=2)

plot(com$cap_ft,com$cap.ft.bulk, 
     pch=21, col="black",bg= "green")

abline(a=0, b=1, lwd=2)

plot(com$cap_tlp,com$cap.tlp.bulk, 
     pch=21, col="black",bg= "green")

abline(a=0, b=1, lwd=2)

dev.off()


# output results ----------------------------------------------------------

write.csv(pv_leaf_uniques, here("inst/extdata/pv_uniques.csv"), row.names=F)

write.csv(pv_params_byleaf,here('inst/extdata', 'leafparams_sum.csv'), row.names = F)

write.csv(param_sum, here("inst/extdata", "sum_params.csv"))

write.csv(pv_params, here("inst/extdata", "pvparams.csv"))