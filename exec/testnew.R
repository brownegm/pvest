## testing new osmotic estimates with quag

library(pvest)


# new fx ------------------------------------------------------------------

# calc pressure potential 
psi_p <- function(r, psi_pi_sat, r_tlp, b){
  p <- -psi_pi_sat * ((r - r_tlp) / (1 - r_tlp))^b
  return(p)
}

imp_b <- function(mod, rwctlp, psitlp){
  return((mod *(rwctlp))/(-rwctlp* psitlp))
}


# calculations ------------------------------------------------------------


quag1 <- pvest::quag |>
  dplyr::filter(leaf == 1)

rwc <- pvest::estRWC(
  quag1,
  fw.index = "fresh.weight",
  wp.index = "water.potential",
  dm.index = "dry.weight",
  n_row = 5, silent = F
)

rwcdf <- attr(rwc, "df")

# estimate osmotic potential
forpio <- tail(rwcdf,4)

pioEst <- estpio(forpio$rwd, -1/forpio$water.potential)

pio <- pioEst$pio

# apoplastic fraction
af <- (pioEst$sma_mod$intercept / pioEst$sma_mod$slope) + 100 

# af is the x intercept of the relationship between rwd and negative inverse water potential
# to put it into terms of relative water content ADD 100

# symplastic relative water content

srwc_num <- rwcdf$rwc/100 - af/100
srwc_den <- 1 - af/100
srwc <- srwc_num/srwc_den
srwc 

srwd <- 1-srwc

# solute potential 
psio <- pio / srwc
max_psip <- -1 * pio
temppsip <- rwcdf$water.potential - psio

# at turgor loss the pressure potential is zero so the water potential there equals tlp
tlp_guess <- psio[6] # this is a guess, we will refine it later

srwc_tlp <- (pio) / (tlp_guess)

sym_modulus <- max_psip / (1-srwc_tlp)

b <- imp_b(sym_modulus, rwctlp = srwc_tlp, psitlp = tlp_guess)
pp <- psi_p(srwc, pio, srwc_tlp, b) 
pp[is.na(pp)] <- 0 # replace NAs with 0
psiest <- pp + psio

## this is based on the values from the quag1 data and 
## based on pi at full turgor.  

test_df |>
  mutate( 
    apoplasticfraction = 100 + (test$sma_mod$slope / test$sma_mod$intercept),
    symplasticrelativewatercontent = ((test_df$rwc - apoplasticfraction) / (100 - apoplasticfraction)),
    srwd = 1 - symplasticrelativewatercontent,
    solutepotentialold = -1/test$sma_mod$intercept + (test$sma_mod$slope * (srwd)),
    solutepotential = pi_ft/(symplasticrelativewatercontent), 
    symrwdtlp = -(test$sma_mod$intercept / test$sma_mod$slope)/100,
    symrwctlp = 100 - symrwdtlp,
    symmodulus = max_psip / (symrwctlp),
    psitlp = -1 / (test$sma_mod$slope * (1-symrwctlp)+ test$sma_mod$intercept), 
    pressurepotential = -(symrwctlp * psitlp) *((symplasticrelativewatercontent-symrwctlp)/(1-symrwctlp))^((symmodulus *(1- symrwctlp))/(-symrwctlp* psitlp)), 
    testpsiold = pressurepotential + solutepotentialold, 
    testpsinew = pressurepotential + solutepotential)



