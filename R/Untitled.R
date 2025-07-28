# 0) assume `df` has columns:
#    - RWD   : relative water deficit (0–1)
#    - psi_s : measured osmotic potential (negative, MPa)

# 1) ESTIMATE r_tlp FROM A SIMPLE LINEAR FIT -------------------------------

lm_tlp <- lm(psi_s ~ RWD, data = df)
coef_tlp <- coef(lm_tlp)
rwd_tlp    <- - coef_tlp["(Intercept)"] / coef_tlp["RWD"]
r_tlp      <- 1 - rwd_tlp
message("Estimated r_tlp = ", round(r_tlp, 3))

rwc_tlp = intercept of the line below turgor
# 2) COMPUTE RELATIVE WATER CONTENT ----------------------------------------

df$r <- 1 - df$RWD


# 3) ESTIMATE ψ_s,ft FROM THE ELASTIC (PRE‐TLP) BRANCH ---------------------

# 3a) subset to r > r_tlp
df_pre <- subset(df, r > r_tlp)

# 3b) fit psi_s ~ 1/r (inverse linear) to get slope = ψ_s,ft
#     we force intercept = 0 so that psi_s = -ψ_s,ft * (1/r)
lm_ft <- lm(I(-psi_s) ~ 0 + I(1/r), data = df_pre)
psi_s_ft <- coef(lm_ft)["I(1/r)"]
message("Estimated |ψ_s,ft| = ", round(psi_s_ft, 3), " MPa")


# 4) FIT POWER‐LAW EXPONENT ON PRE‐TLP DATA -------------------------------

fit_pre <- nls(
  psi_s ~ -psi_s_ft * r^(-b),
  data  = df_pre,
  start = list(b = 1)
)
b_pre <- coef(fit_pre)["b"]
message("Fitted pre‐TLP exponent b = ", round(b_pre, 3))


# 5) FIT POWER‐LAW EXPONENT ON POST‐TLP DATA ------------------------------

# 5a) subset to r ≤ r_tlp
df_post <- subset(df, r <= r_tlp)

fit_post <- nls(
  psi_s ~ -psi_s_ft * ((r - r_tlp)/(1 - r_tlp))^c,
  data  = df_post,
  start = list(c = 2)
)
c_post <- coef(fit_post)["c"]
message("Fitted post‐TLP exponent c = ", round(c_post, 3))


# 6) DEFINE PIECEWISE ψ_s FUNCTION ----------------------------------------

psi_s_nl <- function(r){
  ifelse(
    r > r_tlp,
    -psi_s_ft * r^(-b_pre),                                      # elastic
    -psi_s_ft * ((r - r_tlp)/(1 - r_tlp))^c_post                 # inelastic
  )
}


# 7) APPLY TO NEW DATA -----------------------------------------------------

new_rwd <- c(0.05, 0.20, 0.40, 0.60)
new_r   <- 1 - new_rwd
data.frame(
  RWD      = new_rwd,
  RWC      = new_r,
  psi_s_nl = psi_s_nl(new_r)
)
