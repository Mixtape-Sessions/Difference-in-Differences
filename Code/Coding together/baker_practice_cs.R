# ------------------------------------------------------------------------------
# name: baker_practice_cs.R
# author: scott cunningham (baylor)
# description: coding together to implement callaway and sant'anna on the baker 
#              simulated dataset
# last updated: february 19, 2022
# ------------------------------------------------------------------------------

# load in the packages
library(readstata13)
library(ggplot2)
#install.packages("did")
#devtools::install_github("bcallaway11/did")
library(did) # Callaway & Sant'Anna

# First step: load the data in, call it baker
baker <- data.frame(read.dta13('https://github.com/scunning1975/mixtape/raw/master/baker.dta'))

# Second step. The did package in R requires that the "never treated units" (we have 30)
# be given a zero for their treatment date. In the baker dataset, if you are not treated
# then you do not have a treatment date, but everyone is treated eventually in the baker dataset
# so this next step is technically unnecessary, but it's good to always do it imo. 
# Brant's did package REQUIRES never-treated be given a zero
# for their treatment date. treat_date is the treatment date variable.
# This is just like in Stata: replace treat_date=0 if treat_date==.
baker$treat_date[is.na(baker$treat_date)] <- 0 # untreated units have effective year of 0

# Estimating the effect of our treatment on y (dynamic DGP)
atts <- att_gt(yname = "y", # LHS variable
               tname = "year", # time variable
               idname = "id", # id variable
               gname = "treat_date", # first treatment period variable
               data = baker, # data
               xformla = NULL, # no covariates
               # xformla = ~ X_Iyear_2010, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyettreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "id", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional

# Aggregate ATT (uniform weighted average)
agg_effects1 <- aggte(atts, type = "simple")
summary(agg_effects1)

# Aggregate ATT (weighted average over group ATT(g))
agg_effects2 <- aggte(atts, type = "group")
summary(agg_effects2)

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)
