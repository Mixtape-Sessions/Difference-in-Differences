###################################################################################
# Program:       Effect of treatmet on outcome using baker dataset
# Author:        Scott Cunningham
# Affiliation:   Baylor University
# Created:       11/2/2021
# Date Modified: 11/2/2021
# Modified by:   Scott Cunningham 
###################################################################################


# Libraries ---------------------------------------------------------------

## Install (if necessary) and load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, gsynth, panelView, readstata13)


# Data --------------------------------------------------------------------

# Read in the baker dataset from the web
baker <- data.frame(read.dta13("/users/scott_cunningham/dropbox/CI Workshop/data/baker.dta"))

## Untreated units (currently "NA") should have an effective year of 0
baker$treat_date = replace_na(baker$treat_date, 0)

## It can only estimate the effect if there are no untreated units
## so I have to figure out how drop the post 2004 period
baker <- baker[baker$year<2004,]

## Double check that I have all four groups and 6000. It needs
## to have dropped all the post 2004 observations
table(baker$treat_date)

# Quick look at the data. We have a balanced panel with 1000 treated units (i.e.
# states) and variable treatment timing
panelView(y ~ treat, data = data.frame(baker),
          index = c('id', 'year'), pre.post = TRUE, by.timing = TRUE)

# Similar, but in tabular form
baker %>%
  group_by(id) %>%
  mutate(treated = any(treat>0)) %>%
  group_by(treated) %>%
  summarise(
    n_states = n_distinct(id),
    n_cities = n(),
    mean_treatment_perc = mean(treat)*100 ## Percentage of treated periods
  )


# Matrix completion -------------------------------------------------------

# This will create an object called "reg1" that will contain all the values
# listed in `gsynth`
reg1 = gsynth(y ~ treat,                # "regress" log homicides on post treated status
              data = baker,            # specify our dataset
              index = c("id", "year"),  # Our panel unit and time FEs
              estimator = "mc",         # NB: Sets estimation method to matrix completion!
              nlambda = 10,             # Number of lambda to search
              CV = TRUE,                # Runs cross-validation to choose lambda
              k = 10,                   # Number of folds for cross-validation
              force = "two-way",        # Unit and time fixed effects
              se = TRUE,                # Compute standard errors
              nboots = 1000,            # Number of bootstraps to run
              na.rm = TRUE,             # Remove missing values
              parallel = TRUE,          # Run parallel computing (should decrease time)
              seed = 511235)            # Seed for reproducibility

# Let's check the output
reg1

# In plot form
plot(reg1)

# Alternative representation
plot(reg1, type = "counterfactual", raw = "all")

## We can also extract the overall average treatment effect
reg1$est.avg

## And the cross-validation lambda (i.e. optimal hyper-parameter chosen via CV)
reg1$lambda.cv