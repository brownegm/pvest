library(tidyr)
library(dplyr)
library(readxl)
library(here)

# 1. Data Input ----------------------------------------------------------

# Load all raw pv curve data
pv_data_for_testing <- readxl::read_xlsx(here(
  "inst",
  "extdata",
  "pvldcurvedata_10042022.xlsx"
)) |>
  select(
    !c(fresh.weight.saturated, water.potential.bar, bag.weight, sp.leaf)
  )

# Load pv data summaries by leaf from Browne et al. 2023
itvpv <- readr::read_csv(here("inst/extdata/pvdat.csv")) |>
  # filter these out for the moment they add complexity
  filter(!grepl(c("PLRA|ENFA|BAGA|QUAG|RAIN|CEBE"), spcode))

# rename individuals for CLLA to consistent numbering scheme
itvpv[itvpv$spcode == "CLLA", "individual"] <- as.character(c(1:6))
itvpv[itvpv$spcode == "CLLA", "spcind"] <- paste0("CLLA", c(1:6))

# match IDs of est output
itvpv <- itvpv |>
  mutate(spcind = tolower(spcind), spcode = tolower(spcode))

# create vector for filtering of matching species observations
unique.ids <- pv_data_for_testing |>
  mutate(spcind = paste0(species, leaf)) |>
  dplyr::filter(spcind %in% itvpv$spcind) |>
  pull(spcind) |>
  unique()

# 2. Create Output Data

save(
  pv_data_for_testing,
  itvpv,
  unique.ids,
  file = here("exec/pkgTest", "pv_dataframes_for_comparison.rda")
)
