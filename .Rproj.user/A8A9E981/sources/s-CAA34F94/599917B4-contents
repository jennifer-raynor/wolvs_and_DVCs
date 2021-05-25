# ------------------------------------------------------------------------------
# create independent variable selection matrix
# ------------------------------------------------------------------------------

# use a selection matrix so we can run all regressions programmatically
# this file defines what independent variables are included,
# what years are included,
# and what, if any, observations are dropped
indep_vars.m <- as.matrix(read.csv("regressions/indep_vars_dvcs.csv"))

# convert to logical
# exclude model id because it is character format
indep_vars_logic.m <- apply(indep_vars.m[ , 2:ncol(indep_vars.m)], 2, as.logical)

# use model numbers as row names
row.names(indep_vars_logic.m) <- indep_vars.m[ , "model_id"]

rm(indep_vars.m)

# rename the year by wolf variable to match actual variable names
colnames(indep_vars_logic.m) <- gsub("year_by_wolf", "year * ever_wolves_88_10",
                               colnames(indep_vars_logic.m))

# ------------------------------------------------------------------------------
# functions for regressions
# ------------------------------------------------------------------------------

# loop through all regressions in the matrix above
reg.f <- function(reg_row, data){

  # save selection vectors
  logical.v <- indep_vars_logic.m[reg_row, ]

  logical_cov.v <- logical.v[c("wolf_dum", "wolf_pop_tens",
                               "ln_precip_inch", "ln_temp_minfb32", "ln_temp_maxfg80",
                               "Lln_deer_pop_prehunt",
                               "year", "year * ever_wolves_88_10")]

  # get data set
  # just doing this so we can more easily point it to another data set, if desired
  data.df <- data

  # all code below is flexible enough to handle any regression through 2016,
  # even though only a few of these are presented in the paper

  # subset to desired time period
  if(logical.v["y88_10"]) {data.df <- data.df[data.df$year %in% 1988:2010, ]}
  if(logical.v["y88_16"]) {data.df <- data.df[data.df$year %in% 1988:2016, ]}

  # subset to desired counties - remove multi entry/exit
  if(logical.v["excl_exit_ever"] & logical.v["y88_10"]) {data.df <- data.df[which(data.df$wolf_exit_cty_88_10 != 1), ]}
  if(logical.v["excl_exit_ever"] & logical.v["y88_16"]) {data.df <- data.df[which(data.df$wolf_exit_cty != 1), ]}

  # subset to desired counties - remove boundaries
  if(logical.v["excl_boundary_ever"] & logical.v["y88_10"]) {data.df <- data.df[which(data.df$wolf_boundary_ever_88_10 != 1), ]}
  if(logical.v["excl_boundary_ever"] & logical.v["y88_16"]) {data.df <- data.df[which(data.df$wolf_boundary_ever != 1), ]}

  # put together the regression formula
  reg_formula.v <- as.formula(paste("ln_vc_total", "~",
                                    paste(c(names(logical_cov.v)[logical_cov.v],
                                            paste(names(logical_cov.v)[logical_cov.v], "* dvc_dum"),
                                            "dvc_dum", "cty_fp * dvc_dum"),
                                          collapse="+")))
  # run the regression
  # all regressions include year effects, so we don't need to index on year specifically
  reg <- plm(reg_formula.v,
             data = data.df,
             model = "within",
             index = c("cty_fp"),
             effect = "individual")

  # grab some summary stats
  r2 <- r.squared(reg)
  n <- nobs(reg)

  # calculate robust se's
  reg_clust <- coeftest(reg, vcov=vcovHC(reg, type="sss", cluster="group"))

  # grab coefs, summary stats, and se's for independent var's of interest
  beta_se.df <- data.frame(reg_clust[str_detect(rownames(reg_clust), "wolf_dum:dvc|wolf_pop_tens:dvc|Lln_deer_pop_prehunt:dvc"), 1:2, drop = F],
                           r2 = r2,
                           n = n)

  # add in model number so that we can easily subset by model later
  beta_se.df$model_id <- row.names(indep_vars_logic.m)[reg_row]

  # add in indicator for which wolf variable each coefficient refers to; sometimes have both
  beta_se.df$x_var <- rownames(beta_se.df)

  return(beta_se.df)

}

clean_betas.f <- function(reg_results){

  # combine all regressions estimates into a dataframe
  reg_results.df <- do.call(bind_rows, reg_results)

  # combine coef estimates with logical table
  # we're only saving wolf & deer estimates, but want to know the full list of controls
  reg_results.df <- left_join(reg_results.df,
                              cbind(data.frame(indep_vars_logic.m), model_id = row.names(indep_vars_logic.m)),
                              by = "model_id")

  # clean up names
  names(reg_results.df) <- gsub("Estimate", "coef", names(reg_results.df))

  names(reg_results.df) <- gsub("Std..Error", "se", names(reg_results.df), fixed = T)

  return(reg_results.df)

}