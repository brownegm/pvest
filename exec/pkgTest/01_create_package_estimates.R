# Run package functions to create comparable estimate of PV parameters

# Create space ------------------------------------------------------------
library(tidyverse)
library(here)
library(pvest)

# Load data frames ---------------------------------------------------------------
load(here("exec/pkgTest", "pv_dataframes_for_comparison.rda"))

# Compute parameter estimates ---------------------------------------------

# filter the unique ids that aren't in the summarized df
pvData <- pv_data_for_testing |>
  filter(paste0(species, leaf) %in% unique.ids)

# creating a test dataframe to check the package functions
# this will likely change when the species that fail are fixed

test <- pvData |>
  filter(
    species %in%
      c(
        "alma",
        "alrh",
        "arba",
        "aruv",
        "baga",
        "baga_mb",
        "beoc",
        "casa",
        "cebe",
        "ceoc",
        "codi",
        "cece",
        "clis",
        "clla",
        "clli",
        "enca",
        "enfa",
        "enfa_gj",
        "frdi",
        "hear",
        "heca",
        "hesp",
        "laca"
      )
  )

testPV <- estPV(
  test,
  species,
  leaf,
  fresh.weight,
  water.potential,
  dry.weight
)

# combine outputs into dataframe
pv_params <- purrr::list_rbind(testPV)

combined_manual_auto_estimates <- full_join(
  itvpv,
  pv_params,
  by = join_by(spcind == ids),
  suffix = c("", "_est")
)

save(
  combined_manual_auto_estimates,
  testPV,
  file = here("exec/pkgTest", "combined_pv_estimates.rda")
)
