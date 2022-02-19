#-------------------------------------------------------------------------------
# name: subab.R
# last updated: friday february 18, 2022
#-------------------------------------------------------------------------------

# Load packages
#-------------------------------------------------------------------------------
# Libraries
library(haven)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(bacondecomp)
library(TwoWayFEWeights)
library(fixest)
library(glue)
library(DIDmultiplegt)

# Simple DiD example
data(base_stagg)
head(base_stagg)

# Note that the year_treated is set to 1000 for the never treated
table(base_stagg$year_treated)
table(base_stagg$time_to_treatment)

# The DiD estimation
res_sunab = feols(y ~ x1 + sunab(year_treated, year), base_stagg)
etable(res_sunab)

# By default the reference periods are the first year and the year before the treatment
# i.e. ref.p = c(-1, .F); where .F is a shortcut for the first period.
# Say you want to set as references the first three periods on top of -1

res_sunab_3ref = feols(y ~ x1 + sunab(year_treated, year, ref.p = c(.F + 0:2, -1)), base_stagg)

# Display the two results
iplot(list(res_sunab, res_sunab_3ref))

# ... + show all refs
iplot(list(res_sunab, res_sunab_3ref), ref = "all")

#
# ATT
#

# To get the total ATT, you can use summary with the agg argument:
summary(res_sunab, agg = "ATT")

# You can also look at the total effect per cohort
summary(res_sunab, agg = "cohort")

#
# Binning
#

# Binning can be done in many different ways

# binning the cohort
est_bin.c   = feols(y ~ x1 + sunab(year_treated, year, bin.c = 3:2), base_stagg)

# binning the period
est_bin.p   = feols(y ~ x1 + sunab(year_treated, year, bin.p = 3:1), base_stagg)
#> NOTE: 1 observation removed because of NA values (RHS: 1).

# binning both the cohort and the period
est_bin     = feols(y ~ x1 + sunab(year_treated, year, bin = 3:1), base_stagg)
#> NOTE: 1 observation removed because of NA values (RHS: 1).

# binning the relative period, grouping every two years
est_bin.rel = feols(y ~ x1 + sunab(year_treated, year, bin.rel = "bin::2"), base_stagg)

etable(est_bin.c, est_bin.p, est_bin, est_bin.rel, keep = "year")

