## ======================================================================================================================= ##
## Script:    SENSITIVITY
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Thu Feb 12 09:20:54 2026
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

# clean environment
rm(list = ls())

## SETUP ====================================================================================================================

library(renv)
renv::restore()

set.seed(777) # set seed for random processes

# load required packages
library(here)
library(purrr)
library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(ggpubr)
library(ggdist)
library(ggExtra)
library(viridis)
library(ggplot2)
library(ggpmisc)
library(stringr)
library(magrittr)

# plot settings
options(ggplot2.continuous.colour = "viridis")
options(ggplot2.continuous.fill = "viridis")
theme_set(theme_classic())

# custom functions loaded
source(here::here("code", "functions", "fun_plots.R"))
source(here::here("code", "functions", "fun_helper.R"))
source(here::here("code", "functions", "fun_load_model_results.R"))

### Load Data ---------------------------------------------------------------------------------------------------------------
# dd_dat             = contains behavioural task data (list)
# dd_w02 ... dd_w06   = contains behavioural task data for each wave (df)
# datq05 ... datq07   = contains questionnaire results (processed in lcid_dd_preprocessing.R) (df)
# dat_qdem.           = contains merged questionnaire and demographics data (df)
# ddtinvar_wide       = contains time-invariant master dataset (wide format) (df)
# ddtinvar_long       = contains time-invariant master dataset (long format) (df)
# ddtvar_wide         = contains time-variant master dataset (wide format) (df)
# ddtvar_long         = contains time-variant master dataset (long format) (df)
# mod_results         = contains dataframes of each measurement wave model parameters (list)

# demographics data and task data loaded
dat_demographics <- read_csv(here::here("data", "processed", "demographics.csv"))
dd_master_df <- dd_dat <- readRDS(here::here("data", "processed", "dd_task.Rds"))

# trial level choice data
dd_choice <- read_rds(file = here::here("data", "processed", "dd_choice_data.RDS"))

# masterdata including model results
ddtvar_wide <- utils::read.csv(file = here::here("data", "processed", "tvar_masterdat_wide.csv"), header = TRUE)
ddtvar_long <- utils::read.csv(file = here::here("data", "processed", "tvar_masterdat_long.csv"), header = TRUE)

## DELAY DISCOUNTING DATA ===================================================================================================

# The Leiden Consortium on Individual Development included a monetary delay discounting task at measurement occasions two
# to five. Although there is some evidence supporting the use of monetary tasks and demonstrating converging findings across
# different reward types, it is worth examining whether data from earlier waves provided meaningful results, and whether
# later measures in particular (five to six) which were used in the longitudinal analyses are reliable.

#### ICC across waves -------------------------------------------------------------------------------------------------------

# ICC waves 5 and 6
icc_results <- ddtvar_wide %>%
  dplyr::select(w05_estimate_k, w06_estimate_k) %>%
  drop_na() %>%
  psych::ICC()

data.frame(
  Type = icc_results$results$type,
  ICC = sprintf("%.3f", icc_results$results$ICC),
  F = sprintf("%.3f", icc_results$results$F),
  df1 = icc_results$results$df1,
  df2 = icc_results$results$df2,
  p = sprintf("%.3f", icc_results$results$p),
  Lower_CI = sprintf("%.3f", icc_results$results$`lower bound`),
  Upper_CI = sprintf("%.3f", icc_results$results$`upper bound`)
)

# ICC across waves

# ICC waves 5 and 6
icc_results_all <- ddtvar_wide %>%
  dplyr::select(contains("estimate_k")) %>%
  drop_na() %>%
  psych::ICC()

data.frame(
  Type = icc_results_all$results$type,
  ICC = sprintf("%.3f", icc_results_all$results$ICC),
  F = sprintf("%.3f", icc_results_all$results$F),
  df1 = icc_results_all$results$df1,
  df2 = icc_results_all$results$df2,
  p = sprintf("%.3f", icc_results_all$results$p),
  Lower_CI = sprintf("%.3f", icc_results_all$results$`lower bound`),
  Upper_CI = sprintf("%.3f", icc_results_all$results$`upper bound`)
)

