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

# 319 rows untreated, 11 treated in 2005, etc.
table(castle$effyear)
# max leads are 9; max lags are 5
table(castle$time_til)

# The DiD estimation
castle_sa = feols(l_homicide ~ sunab(effyear, 
                                     year,
                                     ref.c = NULL,
                                     ref.p = -1,
                                     att = TRUE,
                                     no_agg = FALSE), castle)

etable(castle_sa)

#
# ATT
#

# To get the total ATT, you can use summary with the agg argument:
summary(castle_sa, agg = "ATT")

# You can also look at the total effect per cohort
summary(castle_sa, agg = "cohort")
