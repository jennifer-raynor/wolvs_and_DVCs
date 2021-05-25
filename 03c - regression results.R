library(plm)
library(lmtest)
library(tidyverse)

rm(list = ls())

# ------------------------------------------------------------------------------
# inputs
# ------------------------------------------------------------------------------

# get regression functions and selection matrix
source("03b - regression functions.R")

# read full dataset
wolf_dvc_3d_nodrops.pdf <- readRDS("regressions/03a_out - 3d data - no drops.rds")

# read clean dataset
wolf_dvc_3d.pdf <- readRDS("regressions/03a_out - 3d data.rds")

# ------------------------------------------------------------------------------
# run regressions
# ------------------------------------------------------------------------------

# full data - Fig S2
reg_results_nodrops.l <- map(1:nrow(indep_vars_logic.m), reg.f, data = wolf_dvc_3d_nodrops.pdf)

reg_results_nodrops.df <- clean_betas.f(reg_results_nodrops.l)

# clean data - Figs 3, S1, S3
reg_results.l <- map(1:nrow(indep_vars_logic.m), reg.f, data = wolf_dvc_3d.pdf)

reg_results.df <- clean_betas.f(reg_results.l)

# ------------------------------------------------------------------------------
# outputs
# ------------------------------------------------------------------------------

# full data regression results
saveRDS(reg_results_nodrops.df, "regressions/03c_out - reg results nodrops.rds")

# clean data regression results
saveRDS(reg_results.df, "regressions/03c_out - reg results clean.rds")