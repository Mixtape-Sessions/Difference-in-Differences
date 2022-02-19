#-------------------------------------------------------------------------------
# name: castle_dcdh.R
# author: pedro sant'anna (microsoft, vanderbilt) from codechella
# description: implements dCdH with castle doctrine data
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
#-------------------------------------------------------------------------------
# Load Cheng and Hoekstra (2013)
castle <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/castle.dta")
# Do some data manipulation
# replace NA treatment_date with Inf
castle$treatment_date = ifelse(is.na(castle$treatment_date), Inf, castle$treatment_date)
# Create treatment dummy: 1 if treated by that year, 0 otherwise
castle$treated <- as.numeric(castle$year >= castle$treatment_date)

#-------------------------------------------------------------------------------
# Groups: 2005, 2006, 2007, 2008, 2009, Never treated
# Outcome: logged homicides (l_homicide)
# Start analysis by running TWFE and get the coefficient
twfe <- fixest::feols(l_homicide ~ treated | sid+year, 
                      data = castle,
                      cluster = ~sid)

summary(twfe)
#-------------------------------------------------------------------------------
# Now let's get the Bacon decomposition
df_bacon <- bacon(l_homicide ~ treated,
                  data = castle,
                  id_var = "sid",
                  time_var = "year")
# Notice all the weights are positive.  Remember --> that's what Bacon always is
#-------------------------------------------------------------------------------
# Get dCdH decomposition
dCDH_decomp <- twowayfeweights(
    df=castle,
    Y = "l_homicide",
    G = "sid",
    T = "year",
    D = "treated",
    cmd_type = "feTR"
)

# Weakly positive weights
dCDH_positive <- sum(dCDH_decomp$weight[dCDH_decomp$weight>=0])

# Negative weights
dCDH_negative <- sum(dCDH_decomp$weight[dCDH_decomp$weight<0])
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Estimate the effect of a treatment on an outcome using did_multiplegt
did_multiplegt(df = castle,
               Y = "l_homicide",
               G = "sid",
               T = "year",
               D = "treated",
               dynamic = 0,
               placebo = 0)

# Bootstrap
did_multiplegt(df = castle, 
               Y = "l_homicide",
               G = "sid",
               T = "year",
               D = "treated",
               placebo = 0,
               dynamic = 0,
               brep=10)
