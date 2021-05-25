library(plm)
library(lmtest)
library(tidyverse)

rm(list = ls())

# ------------------------------------------------------------------------------
# parameters
# ------------------------------------------------------------------------------

# read in source data directly from Dryad
wolf_dvc.df <- read.csv("https://datadryad.org/stash/downloads/file_stream/678854")

# ------------------------------------------------------------------------------
# clean up data
# ------------------------------------------------------------------------------

# convert to a panel data frame
wolf_dvc.pdf <- pdata.frame(wolf_dvc.df, index = c("cty_fp", "year"))

rm(wolf_dvc.df)

# scale wolf population so the estimated coefficient is easier to interpret
# save wolf as tens so coefficients look nicer
wolf_dvc.pdf$wolf_pop_tens <- wolf_dvc.pdf$wolf_pop / 10

# save lagged ln_deer_pop_prehunt because otherwise we lose observations for first year
# (i.e. deer population go back further than other data)
wolf_dvc.pdf$Lln_deer_pop_prehunt  <- plm::lag(wolf_dvc.pdf$ln_deer_pop_prehunt, 1, shift = "time")

# ------------------------------------------------------------------------------
# prepare data for triple differences regression
# ------------------------------------------------------------------------------

# reshape data so that the regression can appropriately incorporate interactions
wolf_dvc_3d.pdf <- wolf_dvc.pdf %>%
  pivot_longer(c("dvc_total", "non_dvc_total"), names_to = "vc_type", values_to = "vc_value") %>%
  mutate(dvc_dum = ifelse(vc_type == "dvc_total", 1, 0)) %>%
  select(-ln_dvc_total, -ln_non_dvc_total, -dvc_share)

rm(wolf_dvc.pdf)

# calculate log of DVC
# this is the preferred transformation for the dependent variable
wolf_dvc_3d.pdf <- wolf_dvc_3d.pdf %>%
  mutate(ln_vc_total = log(vc_value))

# ------------------------------------------------------------------------------
# drop data with structural breaks
# ------------------------------------------------------------------------------

# TODO: explain here what the data quality concern is
# these are dropped due to data quality concerns
# robustness checks evaluate the effect of keeping these observations in the data

# maintain a copy with all data for robustness check
wolf_dvc_3d_nodrops.pdf <- wolf_dvc_3d.pdf

# identify all counties with structural breaks in DVCs
drop_cty.v <- wolf_dvc_3d.pdf %>%
  group_by(cty_fp) %>%
  summarize(breaks = sum(dvc_break, na.rm = T)) %>%
  filter(breaks > 1) %>%
  .[["cty_fp"]]

# drop all counties with structural breaks in DVCs
wolf_dvc_3d.pdf <- wolf_dvc_3d.pdf %>%
  filter(!cty_fp %in% drop_cty.v)

# ------------------------------------------------------------------------------
# outputs
# ------------------------------------------------------------------------------

# save full dataset
saveRDS(wolf_dvc_3d_nodrops.pdf, "regressions/03a_out - 3d data - no drops.rds")

# save clean dataset
saveRDS(wolf_dvc_3d.pdf, "regressions/03a_out - 3d data.rds")
