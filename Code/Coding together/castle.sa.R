#-------------------------------------------------------------------------------
# name: castle_sa.R
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

# syntax at https://lrberge.github.io/fixest/reference/sunab.html
#sunab(
#  cohort,
#  period,
#  ref.c = NULL,
#  ref.p = -1,
#  bin,
#  bin.rel,
#  bin.c,
#  bin.p,
#  att = FALSE,
#  no_agg = FALSE
#)

#sunab_att(cohort, period, ref.c = NULL, ref.p = -1)

# castle doctrine dataset (cheng and hoekstra 2013)
castle <- data.frame(read.dta13('https://github.com/scunning1975/mixtape/raw/master/castle.dta'))
castle$effyear[is.na(castle$effyear)] <- 1000 # untreated units have effective year of 0
castle$time_til[is.na(castle$time_til)] <- -1000 # untreated units have effective year of 0

# 319 rows untreated, 11 treated in 2005, etc.
table(castle$effyear)
# max leads are 9; max lags are 5
table(castle$time_til)

# "Naive" TWFE DiD (note that the time to treatment for the never treated is -1000)
# (by using ref = c(-1, -1000) we exclude the period just before the treatment and 
# the never treated)
res_twfe = feols(l_homicide ~ i(time_til, ref = c(-1, -1000)) | sid + year, castle)

# To implement the Sun and Abraham (2020) method,
# we use the sunab(cohort, period) function
res_sa20 = feols(l_homicide ~ sunab(effyear, year) | sid + year, castle)

# Plot the two TWFE results
iplot(list(res_twfe, res_sa20), sep = 0.5)

# The full ATT
summary(res_sa20, agg = "att")

# Full disaggregation 
summary(res_sa20)

