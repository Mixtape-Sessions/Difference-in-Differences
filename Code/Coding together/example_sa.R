# ------------------------------------------------------------------------------
## name: example_sa.R
## author: nick huntington-klein and volunteers at LOST
## description: this is from an online resource called LOST, on a chatper on did
## https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
## This wonderful R file will estimate a dynamic event study for TWFE and SA
## and then incredibly put it on the same graph!!  Absolutely gorgeous.
# ------------------------------------------------------------------------------

# load packages
library(data.table) ## For some minor data wrangling
library(fixest)     ## NB: Requires version >=0.9.0

# Load and prepare data
dat = fread("https://raw.githubusercontent.com/LOST-STATS/LOST-STATS.github.io/master/Model_Estimation/Data/Event_Study_DiD/bacon_example.csv") 

# Let's create a more user-friendly indicator of which states received treatment
dat[, treat := ifelse(is.na(`_nfd`), 0, 1)]

# Create a "time_to_treatment" variable for each state, so that treatment is
# relative for all treated units. For the never-treated (i.e. control) units,
# we'll arbitrarily set the "time_to_treatment" value at 0. This value 
# doesn't really matter, since it will be canceled by the treat==0 interaction
# anyway. But we do want to make sure they aren't NA, otherwise feols would drop 
# these never-treated observations at estimation time and our results will be 
# off.
dat[, time_to_treat := ifelse(treat==1, year - `_nfd`, 0)]

mod_twfe = feols(asmrs ~ i(time_to_treat, treat, ref = -1) + ## Our key interaction: time × treatment status
                   pcinc + asmrh + cases |                    ## Other controls
                   stfips + year,                             ## FEs
                 cluster = ~stfips,                          ## Clustered SEs
                 data = dat)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')

# Following Sun and Abraham, we give our never-treated units a fake "treatment"
# date far outside the relevant study period.
dat[, year_treated := ifelse(treat==0, 10000, `_nfd`)]

# Now we re-run our model from earlier, but this time swapping out 
# `i(time_to_treat, treat, ref = -1)` for `sunab(year_treated, year)`.
# See `?sunab`.
mod_sa = feols(asmrs ~ sunab(year_treated, year) + ## The only thing that's changed
                 pcinc + asmrh + cases |
                 stfips + year,
               cluster = ~stfips,
               data = dat)

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))
