library(tidyverse)
library(plm)
library(lmtest)

rm(list = ls())

# ------------------------------------------------------------------------------
# parameters
# ------------------------------------------------------------------------------

# read in source data directly from Dryad
wolf_dvc.df <- read.csv("https://datadryad.org/stash/downloads/file_stream/678854")

# ------------------------------------------------------------------------------
# clean up data
# ------------------------------------------------------------------------------

# TODO: explain here what the data quality concern is
# these are dropped due to data quality concerns
# robustness checks evaluate the effect of keeping these observations in the data

# identify all counties with structural breaks in DVCs
drop_cty <- wolf_dvc.df %>%
  group_by(cty_fp) %>%
  summarize(breaks = sum(dvc_break, na.rm = T)) %>%
  filter(breaks > 1) %>%
  .[["cty_fp"]]

# drop all counties with structural breaks in DVCs
wolf_dvc.df <- wolf_dvc.df %>%
  filter(!cty_fp %in% drop_cty)

# ------------------------------------------------------------------------------
# Data for Fig. 2A-D: Trends in key variables
# ------------------------------------------------------------------------------

# aggregate values for the following variables for wolf and non-wolf counties:
# wolf density (per 100km2), deer density (per km2),
# deer-vehicle collisions, non-deer-vehicle collisions
trends.df <- wolf_dvc.df %>%
  group_by(year, ever_wolves) %>%
  summarize(wolf_pop = sum(wolf_pop),
            deer_pop_prehunt = sum(deer_pop_prehunt),
            deer_range_km2 = sum(deer_range_km2),
            dvc_total = sum(dvc_total),
            non_dvc_total = sum(non_dvc_total)) %>%
  mutate(ever_wolves_pretty = ifelse(ever_wolves == 1, "Wolf county", "Non-wolf county"),
         deer_km2_range = deer_pop_prehunt / deer_range_km2,
         wolf_km2_range = wolf_pop / (deer_range_km2/100)) %>%
  select(year, ever_wolves_pretty, wolf_km2_range, deer_km2_range, dvc_total, non_dvc_total) %>%
  gather(variable, value, -year, -ever_wolves_pretty)

# calculate indices, with earliest year = 100
indices.df <- trends.df %>%
  group_by(variable, ever_wolves_pretty) %>%
  filter(!is.na(value)) %>%
  mutate(base_year = min(year),
         value_index = value/value[year == base_year]*100)

# select variables for plotting
# wolf population is not an index because it starts at 0; all other values are
plot_vars.df <- indices.df %>%
  mutate(value_chart = ifelse(variable == "wolf_km2_range", value, value_index)) %>%
  select(-value, -value_index)

# clean up names for plot facet titles
names.lut <- c("wolf_km2_range" = "a. Wolf density",
               "deer_km2_range" = "b. Deer density",
               "dvc_total" = "c. Deer-vehicle collisions",
               "non_dvc_total" = "d. Other vehicle collisions")

plot_vars.df$facet_titles <- factor(names.lut[plot_vars.df$variable],
                                    levels = c("a. Wolf density",
                                               "c. Deer-vehicle collisions",
                                               "b. Deer density",
                                               "d. Other vehicle collisions"))

# save data for later plotting
fig2_data.l <- list(trends = plot_vars.df)

# ------------------------------------------------------------------------------
# Data for Fig 2E: motivating trend in DVC shares
# ------------------------------------------------------------------------------

# only include data for preferred study period and wolf counties
# also excluding wolves with exit at any point so we don't assume longevity of wolf effect
dvc_share_trend.df <- wolf_dvc.df %>%
  filter(year %in% 1988:2010 & ever_wolves_88_10 == 1 & wolf_exit_cty_88_10 != 1)

# calculate averages of dvc share data, centered on entry year,
# for +- 10 years of wolf entry
# 10 is chosen because effects on either end stabilize in this time frame
dvc_share_trend.df <- dvc_share_trend.df %>%
  group_by(years_to_wolves) %>%
  summarize(dvc_share = mean(dvc_share, na.rm = T)) %>%
  gather(variable, value, -years_to_wolves) %>%
  filter(years_to_wolves %in% -10:10)

# clean up facet title for plotting purposes
dvc_share_trend.df$variable <- gsub("dvc_share",
                                    "e. Deer-vehicle collisions (% of total)",
                                    dvc_share_trend.df$variable)

# save to running list of data
fig2_data.l[["dvc_shares"]] <- dvc_share_trend.df

# ------------------------------------------------------------------------------
# Data for Fig 2F: event study
# ------------------------------------------------------------------------------

# as before, only include data for preferred study period and wolf counties
# also excluding wolves with exit at any point so we don't assume longevity of wolf effect
event_study.df <- wolf_dvc.df %>%
  filter(year %in% 1988:2010 & ever_wolves_88_10 == 1 & wolf_exit_cty_88_10 != 1)

# prepare data for event study
# binning >=10 and <=-10
# relevent to -1 being the excluded group in the regression
event_study.df <- event_study.df %>%
  mutate(ytw_bins = ifelse(years_to_wolves <= -10, -10, years_to_wolves),
         ytw_bins = ifelse(years_to_wolves >= 10, 10, ytw_bins),
         ytw_bins = as.factor(ytw_bins),
         ytw_bins_relevel = relevel(ytw_bins, ref = "-1"))

# save data as a panel dataframe
event_study.pdf <- pdata.frame(event_study.df, index = c("cty_fp", "year"))

# run the regression for the event study
reg <- plm(dvc_share ~ ytw_bins_relevel + year,
           data = event_study.pdf,
           model = "within",
           index = c("cty_fp"),
           effect = "individual")

# restructure coefficients and se's for plotting
event_study_coefs.df <- data.frame(coeftest(reg, vcov=vcovHC(reg, type="sss", cluster="group"))[,1:2]) %>%
  mutate(var_name = rownames(.)) %>%
  filter(str_detect(.[,"var_name"], "ytw")) %>%
  mutate(var_name = str_replace_all(.$var_name, "ytw_bins_relevel", ""),
         y_var = "f. Deer-vehicle collisions (% of total)",
         var_name = as.integer(as.character(var_name)))

names(event_study_coefs.df) <- c("beta", "se", "years_to_wolves", "variable")

# add in a precise zero for excluded base group
# all coefficients are relative to this group
event_study_coefs.df <- rbind(event_study_coefs.df,
                              data.frame(beta = 0,
                                         se = 0,
                                         years_to_wolves = -1,
                                         variable = c("f. Deer-vehicle collisions (% of total)"))) %>%
  arrange(years_to_wolves)

# save to running list of data
fig2_data.l[["event_study"]] <- event_study_coefs.df

# ------------------------------------------------------------------------------
# outputs
# ------------------------------------------------------------------------------

# save final figure data
saveRDS(fig2_data.l, "figures/02a_out - fig2_data.rds")
