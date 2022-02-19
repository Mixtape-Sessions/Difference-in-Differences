#---------------------------------------------------
#      Template for computing event-study-type
#      estimators using Callaway and Sant'Anna (2020)
#      Option to select event window
#---------------------------------------------------
#-----------------------------------------------------------------------------
# CREATED     :   October 5, 2020
# AUTHOR      :   Pedro H. C. Sant'Anna
# AFFILIATION :   Vanderbilt University
# EMAIL       :   pedro.h.santanna@vanderbilt.edu
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Startup - clear memory, load packages, and set parameters
# Clear memory
rm(list=ls())
#-----------------------------------------------------------------------------
# Basic parameters for the procedure - Doesn't change over setups
# Set seed
set.seed(10052020)
#-----------------------------------------------------------------------------
# load the necessary libraries
#Download latest version of did package and load it
devtools::install_github("bcallaway11/did")
library(did)
# Use here package to facilitate relative paths
library(here)
# Use these for data manipulation, and plots
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Function to compute event-study-type parameters using selected event-times
event_study_CS <- function(atts,
                        time = c(-Inf, Inf), 
                        nboot = 1000, 
                        alp = 0.05){
  
  # First, let's run the aggte function in the did package
  es <- did::aggte(atts, type = "dynamic", bstrap = FALSE, alp = alp, clustervars = NULL,
                   cband = FALSE, na.rm = TRUE)
  # do the selection of event times using the "time" argument of the function
  e_select <- (es$egt >= time[1]) & (es$egt <= time[2])
  egt <- es$egt[e_select]
  # get the event-study estimates and their influence functions
  es_att <- es$att.egt[e_select]
  es_inf_function <- es$inf.function$dynamic.inf.func.e[, e_select]
  
  # Now, do the bootstrap
  parameters_boot <- atts$DIDparams
  parameters_boot$biters <- nboot
  parameters_boot$alp <- alp
  parameters_boot$cband <- TRUE
  parameters_boot$bstrap <- TRUE
  bootst_es <- did::mboot(es_inf_function, parameters_boot)
  
  # Compute the aggregated summary measure (average over the positive event-times selected)
  epost <- (es$egt >= time[1]) & (es$egt <= time[2]) & (es$egt >= 0)
  egt_post <- es$egt[epost]
  aggte_es <- base::mean(es$att.egt[epost])
  aggte_es_inf_function <- base::rowMeans(es$inf.function$dynamic.inf.func.e[, epost])
  
  # bootstrap for the aggregated summary measure
  bootst_es_aggte <- did::mboot(aggte_es_inf_function, parameters_boot)
  
  # Put all the results into a tibble
  event_study_out <-
    tibble::tibble(
      event_time      = egt,
      es_coeff        = es_att,
      se_es           = bootst_es$se,
      simult_cv_es    = bootst_es$crit.val,
      pointwise_cv_es = stats::qnorm(1 - (alp/2))
    ) %>%
    mutate(
      lower_pointwise   = es_coeff - pointwise_cv_es * se_es,
      upper_pointwise   = es_coeff + pointwise_cv_es * se_es,
      lower_simult      = es_coeff - simult_cv_es * se_es,
      upper_simult      = es_coeff + simult_cv_es * se_es
    ) %>%
    filter(event_time >= time[1], event_time <= time[2])
  
  out <- list(
    event_time      = egt,
    es_coeff        = es_att,
    se_es           = bootst_es$se,
    simult_cv_es    = bootst_es$crit.val,
    pointwise_cv_es = stats::qnorm(1 - (alp/2)),
    es_aggte        = aggte_es,
    se_aggte_es     = bootst_es_aggte$se,
    out_tibble      = event_study_out
  )
  return (out)
}
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Load the data (here, I am assuming you have it in csv)
dta <- read.csv(here("put_relative_path_of_your_data"))
#-----------------------------------------------------------------------------
# Estimate the ATT(g,t)'s using Callaway and Sant'Anna (2020) procedure
# If focus is on event-study and not on the ATT(g,t)'s per se, set bstrap = FALSE.
# I am assuming you have a balanced panel data. If that is not the case, set panel = FALSE
# I am assuming we are clustering the standard errors in the idname dimension (default in the code)
atts <- att_gt(yname = "name_of_outcome_variable", 
               tname = "name_of_time_variable", 
               idname = "name_of_id_variable",
               first.treat.name = "name_of_first_treatment_variable", 
               data = dta, 
               estMethod = "dr",
               control.group = "notyettreated",
               bstrap = FALSE,
               biters = nboot,
               printdetails = T,
               clustervars = NULL,
               panel = TRUE)
#-----------------------------------------------------------------------------
# Compute event-study
es_CS <- event_study_CS(atts,
                        time = c(-30, 30), 
                        nboot = 1000, 
                        alp = 0.05)
#-----------------------------------------------------------------------------
# Plot event-studies
# You will need to adjust the annotate x and y
plot_es <- ggplot2::ggplot(es_CS$out_tibble,
                           aes(x=event_time, y=es_coeff, ymin = lower_simult,
                               ymax = upper_simult)) +
  geom_line(aes(x=event_time, y=es_coeff), size = 1.5, colour = "red4") +
  geom_hline(yintercept = 0, colour="black", size = 0.5, linetype = "dashed")+
  geom_vline(xintercept = 0, colour="black", size = 0.5, linetype = "dashed")+
  geom_ribbon(aes(ymin= lower_pointwise, 
                  ymax= upper_pointwise), alpha = 0.4) +
  geom_ribbon(aes(ymin= lower_simult, 
                  ymax= upper_simult), alpha = 0.35) +
  theme_minimal()+
  theme(axis.title = element_text(color="black",  size=15))+
  theme(axis.text.y = element_text(size = 12, face = "bold", color="black"))+
  theme(axis.text.x = element_text(size = 12, face = "bold",color="black"))+
  scale_x_continuous("Event time",
                     breaks = seq(min(es_CS$event_time), max(es_CS$event_time), by = 1)) +
  scale_y_continuous("Event-study coefficients", breaks = scales::pretty_breaks()) + 
  annotate(geom="text", x=3, y=0.3, label=paste0("Overall Effect: ", round(es_CS$es_aggte, 4),
                                                 " [",round(es_CS$es_aggte - es_CS$pointwise_cv_es * es_CS$se_aggte_es, 4),
                                                 ", ",
                                                 round(es_CS$es_aggte + es_CS$pointwise_cv_es * es_CS$se_aggte_es, 4),
                                                 "]"))

plot_es




