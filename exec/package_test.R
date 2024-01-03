## Test the package functionality with data from ITV-PV

# Create space ------------------------------------------------------------

library(tidyverse)
library(here)
library(pvest)

# Functions ---------------------------------------------------------------

find_outlier <- function(x, y, index) {
  temp <- merge(x, y)

  var <- names(temp)[index]

  temp_output <- temp %>%
    filter({{ index }} <= lwr | {{ index }} >= upr)

  return(temp_output)
}

# for simplicity in later prediction interval estimation this function creates a new df with just the variable
# to be worked on
f <- function(data, var) {
  new_preds <- data %>% # create df with just the variable of interest
    select(any_of(var)) %>%
    transmute(var = seq(min(data[, var], na.rm = T), max(data[, var], na.rm = T), length.out = nrow(data)))

  names(new_preds)[1] <- {{ var }
  } # rename that column to match

  return(new_preds)
}

# Load data ---------------------------------------------------------------

## individual mass and pv data
pv_dat <- readxl::read_xlsx(here("inst", "extdata", "pvldcurvedata_10042022.xlsx")) %>%
  select(!c(fresh.weight.saturated, water.potential.bar, bag.weight, sp.leaf)) %>%
  as.data.frame()

pvsps <- pv_dat %>% transmute(sp_indiv = paste0(toupper(species), leaf))

## paper data summarized
itvpv <- readr::read_csv(here("inst/extdata/pvdat.csv"))

itvpv <- itvpv %>%
  filter(!grepl(c("PLRA|ENFA|BAGA|QUAG|RAIN|CEBE"), spcode)) # filter these out for the moment they add complexity

itvpv[itvpv$spcode == "CLLA", "individual"] <- as.character(c(1:6))
itvpv[itvpv$spcode == "CLLA", "spcind"] <- paste0("CLLA", c(1:6))

# match IDs of est output
itvpv <- itvpv %>%
  mutate(unique_id = paste(tolower(spcode), individual, sep = "_"), .before = swc)

# only the unique ids (SPECIES{1:X}) for the data
unique.ids <- pvsps %>%
  dplyr::filter(pvsps$sp_indiv %in% itvpv$spcind) %>%
  pull(1) %>%
  unique()

# Compute parameter estimates ---------------------------------------------

# filter the unique ids that aren't in the summarized df
pv_dat_fil <- pv_dat %>%
  filter(paste0(toupper(species), leaf) %in% unique.ids)

test<-pv_dat_fil%>%filter(species=="heca")

# compute pv parameters
pv_params <- estParams(pv_dat_fil,
                       fw.index = 5, wp.index = 4, dm.index = 3, 
                       n_pts = F)

pv_params_r2 <- estParams(pv_dat_fil, 
                          fw.index = 5, wp.index = 4, dm.index = 3,
                          n_pts = T, method ="r2")

pv_params_cv <- estParams(pv_dat_fil, 
                          fw.index = 5, wp.index = 4, dm.index = 3, 
                          n_pts = T, method = "cv")

pv_params_pio <- estParams(pv_dat_fil,
                           fw.index = 5, wp.index = 4, dm.index = 3, 
                           n_pts = T, method ="pio")

pv_params_piecewise <- estParams(pv_dat_fil,
                           fw.index = 5, wp.index = 4, dm.index = 3, 
                           n_pts = T, method ="piecewise")

# find the leaves that have NAs by psi_tlp
og_nas <- pv_params%>%filter(is.na(leaf.waterpotential.attlp))
r2_nas <- pv_params_r2%>%filter(is.na(leaf.waterpotential.attlp))
cv_nas <- pv_params_cv%>%filter(is.na(leaf.waterpotential.attlp))
pio_nas <- pv_params_pio%>%filter(is.na(leaf.waterpotential.attlp))
piece_nas <- pv_params_piecewise%>%filter(is.na(leaf.waterpotential.attlp))

lapply(list(four_points=og_nas,greatestr2=r2_nas, lowestpio=pio_nas,piecewise_reg=piece_nas), function(x) nrow(x))

# Summarize output --------------------------------------------------------

# summarize by leaf
pv_params_byleaf <- pv_params %>%
  select(
    species, leaf, unique_id, saturated.water.content, osm.pot.fullturgor, apoplastic.fraction,
    relative.water.deficit.attlp:cap.tlp.sym
  ) %>%
  group_by(unique_id) %>%
  summarize_if(is.numeric, unique)

uniques <- readxl::read_xlsx(here("inst", "extdata", "summary_main.xlsx"), sheet = "unique_ids") %>% pull(unique_id)

pv_leaf_uniques <- pv_params_byleaf %>%
  filter(unique_id %in% uniques)

# summarize by species

# pv_params_byspecies<-sumParams(pv_params, species, remove.cols = T, cols.to.remove="leaf")#%>%select(species, saturated.water.content:cap.tlp.sym)
#
# itvpv_byspecies<-sumParams(itvpv, spcode)#summarize the manual estimates by species

pv_params_byspecies <- pv_params %>%
  select(!leaf:saturated.water.mass) %>%
  group_by(species) %>%
  summarize(across(where(is.numeric), ~ mean(., na.rm = T)))

itvpv_byspecies <- itvpv %>%
  mutate(spcode = tolower(spcode)) %>%
  filter(spcode %in% pv_params_byspecies$species) %>%
  group_by(spcode) %>%
  summarize(across(where(is.numeric), ~ mean(., na.rm = T))) %>%
  rename(species = spcode)

# combine measured and estimated ------------------------------------------
com <- right_join(itvpv, pv_params_byleaf, by = "unique_id", suffix = c("", "_est"))

com_species <- right_join(itvpv_byspecies, pv_params_byspecies, by = "species", suffix = c("", "_est"))

# Estimate OLS and pred intervals (BY LEAF) ----------------------------------

# variables pred and manual
og_vars <- names(com)[9:20]
pred_vars <- names(com)[c(22:24, 26, 28:35)]

# reorder pred_vars to match og_vars
pred_vars <- pred_vars[c(1, 2, 6, 3:5, 7:8, 9, 11, 10, 12)]
pred_vars_index <- which(names(com) %in% pred_vars)[c(1, 2, 6, 3:5, 7:8, 9, 11, 10, 12)] # as index numbers

# estimate linear models and summaries

pv_lms <- purrr::map2(# compute linear models for each variable pair
  c(9:20), pred_vars_index,
  ~ lm(as.formula(paste(names(com)[.x], "~", names(com)[.y])), data = com)
) 

pv_lms_summary <- purrr::map(pv_lms, \(lm) summary(lm)) # save lm summaries

pv_lms.origin <- purrr::map2(# compute linear models for each variable pair forced through origin
  c(9:20), pred_vars_index,
  ~ lm(as.formula(paste(names(com)[.x], "~", "0+", names(com)[.y])), data = com)
) 

# prep for prediction intervals
# see ?predict.lm
# predictions<-predict(test, interval="prediction")

# f(com, "saturated.water.content")

pv_ <- map(pred_vars_index, \(coln) f(data = com, names(com)[coln])) # use function f to create "newdata" dataframes for each variable

# estimate prediction intervals
pv_pred_intervals <- map2(
  pv_lms,
  pv_,
  ~ predict(.x, newdata = .y, interval = "prediction") %>%
    as.data.frame()
)

pv_pred_intervals.origin <- map2(
  pv_lms.origin,
  pv_,
  ~ predict(.x, newdata = .y, interval = "prediction") %>%
    as.data.frame()
)

# Estimate OLS and pred intervals (BY SPECIES) ----------------------------------

# create separate index for summarized species df 
pred_vars.sp <- pred_vars[c(1, 2, 6, 3:5, 7:8, 9, 11, 10, 12)]
pred_vars_index.sp <- which(names(com_species) %in% pred_vars)[c(1, 2, 6, 3:5, 7:8, 9, 11, 10, 12)] # as index numbers

# estimate linear models and summaries
pv_lms.sp <- purrr::map2(# compute linear models for each variable pair
  c(2:13), pred_vars_index.sp,
  ~ lm(as.formula(paste(names(com_species)[.x], "~", names(com_species)[.y])), data = com_species)
) 

pv_lms_summary.sp <- purrr::map(pv_lms.sp, \(lm) summary(lm)) # save lm summaries


pv_lms.origin.sp <- purrr::map2(# compute linear models for each variable pair fitted through the origin
  c(2:13), pred_vars_index.sp,
  ~ lm(as.formula(paste(names(com_species)[.x], "~", "0+", names(com_species)[.y])), data = com_species)
) 

# prep for prediction intervals
pv_sp <- map(pred_vars_index.sp, \(coln) f(data = com_species, names(com_species)[coln])) # use function f to create "newdata" dataframes for each variable

# estimate prediction intervals
pv_pred_intervals.sp <- map2(
  pv_lms.sp,
  pv_sp,
  ~ predict(.x, newdata = .y, interval = "prediction") %>%
    as.data.frame()
)

pv_pred_intervals.origin.sp <- map2(
  pv_lms.origin.sp,
  pv_sp,
  ~ predict(.x, newdata = .y, interval = "prediction") %>%
    as.data.frame()
)

# Testing for outliers ----------------------------------------------------

## Testing ability to estimate outliers 
pv_pred_intervals.origin

x <- pv_pred_intervals[[1]]
y <- pv_[[1]]
t <- find_outlier(x = pv_pred_intervals[[1]], y = pv_[[1]], 4)

test <- merge(pv_pred_intervals[[1]], pv_[[1]], )

test.filt <- test %>% # these are the saturated water content values that are greater than or less than the pred.int
  filter(saturated.water.content <= lwr | saturated.water.content >= upr)

test <- cbind(pv_pred_intervals[[2]], pv_[[2]])

test.filt <- test %>% # these are the saturated water content values that are greater than or less than the pred.int
  filter(osm.pot.fullturgor <= lwr | osm.pot.fullturgor >= upr)

## another another option is cook's distance
# https://r-statistics.co/Outlier-Treatment-With-R.html
cooks.d<-cooks.distance(pv_lms[[2]])

plot(cooks.d, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooks.d, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooks.d)+1, y=cooks.d, labels=ifelse(cooks.d>4*mean(cooks.d, na.rm=T),names(cooks.d),""), col="red")  # add labels

## find the influential values 
cooks.d_odd <- as.numeric(names(cooks.d)[(cooks.d > 4*mean(cooks.d, na.rm=T))])  # influential row numbers

head(cooks.d_odd)

## this seems to work well; now for all the variables 
cooks.dist_predictions <- lapply(1:length(pv_lms), function(mod) {
  cooks <- cooks.distance(pv_lms[[mod]])
  cooks.odd <- as.numeric(names(cooks)[(cooks > 4*mean(cooks, na.rm=T))])
  return(cooks.odd)
  })

cooks.dist_predictions

# filter dfs by the weird rows
coms_pio.odd <- com%>%
  slice(cooks.dist_predictions[[2]])

plot(coms_pio.odd$osm.pot.fullturgor, coms_pio.odd$pi_o,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
)

# PLOT: Leaf-level predictions -------------------------------------------------------------
pdf(file = here::here("inst/extdata", "pv_params_leaf.pdf"))

plot(com$swc ~ com$saturated.water.content,
  pch = 21, cex = 2,
  col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms[[1]], col = "#4f8359", lwd = 3)

# lines(pv_[[1]][[1]], pv_pred_intervals[[1]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_[[1]][[1]], pv_pred_intervals[[1]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_[[1]][[1]], pv_pred_intervals.origin[[1]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_[[1]][[1]], pv_pred_intervals.origin[[1]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

text(x=com$saturated.water.content, y=com$swc, labels=1:nrow(com)) #find weirdos

plot(com$osm.pot.fullturgor, com$pi_o,
  pch = 21, cex = 2,
  col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms[[2]], col = "#4f8359", lwd = 3)

abline(pv_lms.origin[[2]], col = "#4f8359", lwd = 3)

# lines(pv_[[2]][[1]], pv_pred_intervals[[2]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_[[2]][[1]], pv_pred_intervals[[2]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_[[2]][[1]], pv_pred_intervals.origin[[2]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_[[2]][[1]], pv_pred_intervals.origin[[2]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

text(x=com$saturated.water.content, y=com$swc, labels=1:nrow(com)) #find weirdos

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com$leaf.waterpotential.attlp, com$psi_tlp,
  pch = 21, cex = 2,
  col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms[[3]], col = "#4f8359", lwd = 3)

# lines(pv_[[3]][[1]], pv_pred_intervals[[3]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_[[3]][[1]], pv_pred_intervals[[3]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_[[3]][[1]], pv_pred_intervals.origin[[3]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_[[3]][[1]], pv_pred_intervals.origin[[3]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com$apoplastic.fraction, com$af,
  pch = 21, cex = 2,
  col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms[[4]], col = "#4f8359", lwd = 3)

# lines(pv_[[4]][[1]], pv_pred_intervals[[4]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_[[4]][[1]], pv_pred_intervals[[4]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_[[4]][[1]], pv_pred_intervals.origin[[4]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_[[4]][[1]], pv_pred_intervals.origin[[4]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com$relative.water.content.attlp, com$rwc_tlp,
  pch = 21, cex = 2,
  col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms[[5]], col = "#4f8359", lwd = 3)

# lines(pv_[[5]][[1]], pv_pred_intervals[[5]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_[[5]][[1]], pv_pred_intervals[[5]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_[[5]][[1]], pv_pred_intervals.origin[[5]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_[[5]][[1]], pv_pred_intervals.origin[[5]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com$modulus_est, com$modulus,
  pch = 21, cex = 2,
  col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms[[7]], col = "#4f8359", lwd = 3)

# lines(pv_[[7]][[1]], pv_pred_intervals[[7]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_[[7]][[1]], pv_pred_intervals[[7]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_[[7]][[1]], pv_pred_intervals.origin[[7]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_[[7]][[1]], pv_pred_intervals.origin[[7]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com$modulus_sym, com$mod_sym,
  pch = 21, cex = 2,
  col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms[[8]], col = "#4f8359", lwd = 3)

# lines(pv_[[8]][[1]], pv_pred_intervals[[8]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_[[8]][[1]], pv_pred_intervals[[8]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_[[8]][[1]], pv_pred_intervals.origin[[8]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_[[8]][[1]], pv_pred_intervals.origin[[8]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com$cap.ft.bulk, com$cap_ft,
  pch = 21, cex = 2,
  col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms[[9]], col = "#4f8359", lwd = 3)

# lines(pv_[[9]][[1]], pv_pred_intervals[[9]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_[[9]][[1]], pv_pred_intervals[[9]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_[[9]][[1]], pv_pred_intervals.origin[[9]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_[[9]][[1]], pv_pred_intervals.origin[[9]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com$cap.tlp.bulk, com$cap_tlp,
  pch = 21, cex = 2,
  col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms[[10]], col = "#4f8359", lwd = 3)

# lines(pv_[[10]][[1]], pv_pred_intervals[[10]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_[[10]][[1]], pv_pred_intervals[[10]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_[[10]][[1]], pv_pred_intervals.origin[[10]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_[[10]][[1]], pv_pred_intervals.origin[[10]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

dev.off()


# PLOT: Species-level predictions -----------------------------------------

pdf(file = here::here("inst/extdata", "pv_params_species.pdf"))

plot(com_species$swc ~ com_species$saturated.water.content,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
) 

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms.sp[[1]], col = "#4f8359", lwd = 3)

# lines(pv_sp[[1]][[1]], pv_pred_intervals.sp[[1]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_sp[[1]][[1]], pv_pred_intervals.sp[[1]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_sp[[1]][[1]], pv_pred_intervals.origin.sp[[1]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_sp[[1]][[1]], pv_pred_intervals.origin.sp[[1]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com_species$osm.pot.fullturgor, com_species$pi_o,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms.sp[[2]], col = "#4f8359", lwd = 3)

abline(pv_lms.origin.sp[[2]], col = "#4f8359", lwd = 3)

# lines(pv_sp[[2]][[1]], pv_pred_intervals.sp[[2]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_sp[[2]][[1]], pv_pred_intervals.sp[[2]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_sp[[2]][[1]], pv_pred_intervals.origin.sp[[2]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_sp[[2]][[1]], pv_pred_intervals.origin.sp[[2]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com_species$leaf.waterpotential.attlp, com_species$psi_tlp,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms.sp[[3]], col = "#4f8359", lwd = 3)

# lines(pv_sp[[3]][[1]], pv_pred_intervals.sp[[3]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_sp[[3]][[1]], pv_pred_intervals.sp[[3]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_sp[[3]][[1]], pv_pred_intervals.origin.sp[[3]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_sp[[3]][[1]], pv_pred_intervals.origin.sp[[3]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com_species$apoplastic.fraction, com_species$af,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms.sp[[4]], col = "#4f8359", lwd = 3)

# lines(pv_sp[[4]][[1]], pv_pred_intervals.sp[[4]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_sp[[4]][[1]], pv_pred_intervals.sp[[4]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_sp[[4]][[1]], pv_pred_intervals.origin.sp[[4]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_sp[[4]][[1]], pv_pred_intervals.origin.sp[[4]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com_species$relative.water.content.attlp, com_species$rwc_tlp,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms.sp[[5]], col = "#4f8359", lwd = 3)

# lines(pv_sp[[5]][[1]], pv_pred_intervals.sp[[5]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_sp[[5]][[1]], pv_pred_intervals.sp[[5]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_sp[[5]][[1]], pv_pred_intervals.origin.sp[[5]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_sp[[5]][[1]], pv_pred_intervals.origin.sp[[5]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com_species$modulus_est, com_species$modulus,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms.sp[[7]], col = "#4f8359", lwd = 3)

# lines(pv_sp[[7]][[1]], pv_pred_intervals.sp[[7]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_sp[[7]][[1]], pv_pred_intervals.sp[[7]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_sp[[7]][[1]], pv_pred_intervals.origin.sp[[7]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_sp[[7]][[1]], pv_pred_intervals.origin.sp[[7]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com_species$modulus_sym, com_species$mod_sym,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms.sp[[8]], col = "#4f8359", lwd = 3)

# lines(pv_sp[[8]][[1]], pv_pred_intervals.sp[[8]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_sp[[8]][[1]], pv_pred_intervals.sp[[8]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_sp[[8]][[1]], pv_pred_intervals.origin.sp[[8]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_sp[[8]][[1]], pv_pred_intervals.origin.sp[[8]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com_species$cap.ft.bulk, com_species$cap_ft,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms.sp[[9]], col = "#4f8359", lwd = 3)

# lines(pv_sp[[9]][[1]], pv_pred_intervals.sp[[9]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_sp[[9]][[1]], pv_pred_intervals.sp[[9]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_sp[[9]][[1]], pv_pred_intervals.origin.sp[[9]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_sp[[9]][[1]], pv_pred_intervals.origin.sp[[9]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

plot(com_species$cap.tlp.bulk, com_species$cap_tlp,
     pch = 21, cex = 2,
     col = "black", bg = "#4f8359"
)

abline(a = 0, b = 1, lwd = 2)

abline(pv_lms.sp[[10]], col = "#4f8359", lwd = 3)

# lines(pv_sp[[10]][[1]], pv_pred_intervals.sp[[10]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
# lines(pv_sp[[10]][[1]], pv_pred_intervals.sp[[10]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

lines(pv_sp[[10]][[1]], pv_pred_intervals.origin.sp[[10]][[3]], col = "orangered", lty = "dashed", lwd = 2) # upper
lines(pv_sp[[10]][[1]], pv_pred_intervals.origin.sp[[10]][[2]], col = "orangered", lty = "dashed", lwd = 2) # lower

legend("bottomright", legend = c("1:1", "OLS", "Pred.Int"), lty = c(1, 1, 2), col = c("black", "#4f8359", "orangered"), lwd = 2, bty = "n")

dev.off()

# Output results ----------------------------------------------------------

write.csv(pv_leaf_uniques, here("inst/extdata/pv_uniques.csv"), row.names = F)

write.csv(pv_params_byleaf, here("inst/extdata", "leafparams_sum.csv"), row.names = F)

write.csv(param_sum, here("inst/extdata", "sum_params.csv"))

write.csv(pv_params, here("inst/extdata", "pvparams.csv"))
