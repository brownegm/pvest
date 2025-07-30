

## testing new osmotic estimates with quag

```{r}
library(pvest)

# new fx ------------------------------------------------------------------

# calc pressure potential 
psi_p <- function(r, psi_pi_sat, r_tlp, b){
  p <- -psi_pi_sat * ((r - r_tlp) / (1 - r_tlp))^b
  return(p)
}

psi_p_tlp <- function(r, psi_pi_sat, r_tlp, psitlp, b){
  p <- -(rwctlp * psitlp) * ((r - r_tlp) / (1 - r_tlp))^b
  return(p)
}

imp_b <- function(mod, rwctlp, psitlp){
  return((mod *(1-rwctlp))/(-rwctlp * psitlp))
}


```


```{r}
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
# psio_tlp <- srwc_tlp * psitlp/srwc
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
plot(rwcdf$water.potential~psiest, ylim = c(0, -4), xlim=c(0,-4))
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




### I think I'm doing this too algebraically, I need to think about it more as a solution to a nonlinear model 
```

```{r}
# --- Load required libraries ---
library(minpack.lm)  # For nonlinear regression (nlsLM is more robust than base nls)
library(ggplot2)     # For plotting results

# --- 1. Example data generation (Replace with your own PV data) ---
set.seed(123)  # Ensures reproducibility of simulated data

# Symplastic relative water content (RWC) values (independent variable)
r <- srwc

# Initial guess values
pi_sat <- pio  # Osmotic potential at full turgor (MPa)
rwctlp <- srwc_tlp        # Relative water content at turgor loss point (TLP) guess
b_guess <- sym_modulus            # Elasticity exponent controlling curve shape

# Define the pressure potential function
calc_psi_p <- function(r, pi_sat, r_tlp, b) {
  # For r > r_tlp: calculate Ψp based on nonlinear elastic response
  # For r ≤ r_tlp: Ψp = 0 (no turgor below TLP)
  p <- ifelse(r > r_tlp,
              -pi_sat * ((r - r_tlp) / (1 - r_tlp))^(-pi_sat/r_tlp),
              0)
  return(p)
}

calc_psi <- function(srwc, pio, psio, p){
  
  psi <- p + psio 
  
  return(psi)
  
}

# Simulate observed Ψp data with random noise added
psi_p_sim <-  #calc_psi_p(r=srwc, pi_sat, srwc_tlp, b_guess)

# Combine into a data frame for fitting
df <- data.frame(r = srwc, psi_p = psi_p_sim)

# --- 2. Nonlinear model fitting (Estimate parameters from data) ---
# We use nlsLM (Levenberg–Marquardt) for stability and ability to enforce parameter bounds.

fit <- nlsLM(
  psi_p ~ ifelse(r > r_tlp,
                 -pi_sat * ((r - r_tlp) / (1 - r_tlp))^(b),
                 0),                  # Model equation
  data = df,                          # Data frame
  start = list( # Starting guess for osmotic potential at full turgor
               r_tlp = median(srwc), 
               b = sym_modulus),          # Starting guess for RWC at TLP # Starting guess for elasticity exponent
  lower = c(0.6, 1),            # Lower bounds (physiological constraints)
  upper = c(0.9, 14),              # Upper bounds
  control = nls.lm.control(maxiter = 500)  # Increase iterations for stability
)

btheo <- -pi_sat/1-coef(fit)["r_tlp"]  # Extract fitted elasticity exponent

# --- 3. View fitted parameter estimates ---
summary(fit)  # Full model summary with estimates and SEs

# Extract fitted parameter values
params <- coef(fit)
psi_pi_sat_est <- params["psi_pi_sat"]  # Estimated Ψπ_sat (MPa)
r_tlp_est <- params["r_tlp"]            # Estimated RWC at TLP
b_est <- params["b"]                    # Estimated elasticity exponent

# Print parameter results
cat("Estimated Ψπ_sat (MPa):", psi_pi_sat_est, "\n")
cat("Estimated RWC_tlp:", r_tlp_est, "\n")
cat("Estimated b (elasticity):", b_est, "\n")

# --- 4. Generate predicted Ψp values from fitted model ---
df$psi_p_pred <- predict(fit)  # Predicted Ψp for plotting

# --- 5. Visualization: Observed vs Fitted Curve ---
ggplot(df, aes(x = r)) +
  geom_point(aes(y = psi_p), color = "blue", size = 2) +       # Observed Ψp data
  geom_line(aes(y = psi_p_pred), color = "red", linewidth = 1) +  # Fitted curve
  geom_vline(xintercept = r_tlp_est, linetype = "dashed", color = "gray") + # TLP line
  annotate("text", x = r_tlp_est, y = max(df$psi_p),
           label = paste0("RWC_tlp = ", round(r_tlp_est, 2)), vjust = 1) +
  #scale_y_reverse() +  # Invert y-axis (negative MPa values plotted downward)
  labs(x = "Symplastic Relative Water Content (RWC)",
       y = "Pressure Potential (Ψp, MPa)",
       title = "Pressure-Volume Curve Fit") +
  theme_minimal()


```