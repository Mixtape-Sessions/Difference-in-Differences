#-------------------------------------------------------------------------------
# name: baker_sa.R
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

# baker simulated dataset of 1000 firms and 4 groups
baker <- data.frame(read.dta13("https://github.com/scunning1975/mixtape/raw/master/baker.dta"))
baker$treat_date[is.na(baker$treat_date)] <- 10000 # untreated units have effective year of 0
baker$time_til[is.na(baker$time_til)] <- -1000 # untreated units have effective year of 0

# Four groups, each with 7500 observations
table(baker$treat_date)
# leads and lags run from minimum of -24 to a maximum of +23
table(baker$time_til)

# "Naive" TWFE DiD (note that the time to treatment for the never treated is -10000)
# (by using ref = c(-1, -1000) we exclude the period just before the treatment and 
# the never treated)
res_twfe = feols(y ~ i(time_til, ref = c(-1)) | id + year, baker)

# To implement the Sun and Abraham (2020) method,
# we use the sunab(cohort, period) function
res_sa20 = feols(y ~ sunab(treat_date, year) | id + year, baker)

# Plot the two TWFE results
iplot(list(res_twfe, res_sa20), sep = 0.5)

# The full ATT
summary(res_sa20, agg = "att")

# Full disaggregation 
summary(res_sa20)

