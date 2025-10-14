## ======================================================================================================================= ##
## Script:    MULTIGROUP CONFIRMATORY FACTOR ANALYSIS
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Wed Sep 17 14:45:28 2025
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

library(renv)

renv::activate()

library(lavaan)
library(tidyverse)
library(here)
library(semTools)
library(semPlot)

qdat5 <- read.csv(here::here("data", "processed", "questionnaires_wave05_full.csv"))
qdat6 <- read.csv(here::here("data", "processed", "questionnaires_wave06_full.csv"))
qdat7 <- read.csv(here::here("data", "processed", "questionnaires_wave07_full.csv"))

str(qdat5)

### BISBAS Scale ------------------------------------------------------------------------------------------------------------

# extract BIS item data
qbis5 <- qdat5 %>%
  select(subjID, contains("BISBAS_")) %>%
  select(-contains("Filler")) %>%
  select(-"bisbas_total") %>%
  select(-"bisbas_bas") %>%
  select(-"bisbas_bis")

qbis6 <- qdat6 %>%
  select(subjID, contains("BISBAS_")) %>%
  select(-contains("Filler")) %>%
  select(-"bisbas_total") %>%
  select(-"bisbas_bas") %>%
  select(-"bisbas_bis")

qbis7 <- qdat7 %>%
  select(subjID, contains("BISBAS_")) %>%
  select(-contains("Filler")) %>%
  select(-"bisbas_total") %>%
  select(-"bisbas_bas") %>%
  select(-"bisbas_bis")

# merge raw questionnaire data
qdat_bis <- qbis5 %>%
  full_join(qbis6, by = "subjID", suffix = c("_W5", "_W6")) %>%
  full_join(qbis7, by = "subjID")

# rename for convenience
qdat_bis <- qdat_bis %>%
  rename_with(~{
    wave <- str_extract(.x, "w0?(\\d+)_", group = 1)
    item_num <- str_extract(.x, "_(\\d+)_", group = 1)
    paste0(sprintf("%02d", as.numeric(wave)), "_bisbas_", item_num)
  }, .cols = -subjID)

# remove missing items (fillers and total scores)
qdat_bis_long <- qdat_bis %>%
  pivot_longer(
    cols = -subjID,
    names_to = c("wave", "item"),
    names_pattern = "(\\d+)_bisbas_(\\d+)",
    values_to = "response"
  ) %>%
  mutate(item = as.character(item)) %>%
  mutate(item = as.integer(item)) %>%
  mutate(item = match(item, sort(unique(item))))

qdat_bisbas_wide <- qdat_bis_long %>%
  mutate(item = match(as.integer(item), sort(unique(as.integer(item))))) %>% # relabel items 1–20
  pivot_wider(
    id_cols = c(subjID, wave),
    names_from = item,
    names_prefix = "x",
    values_from = response
  )

#### Confirmatory Model -----------------------------------------------------------------------------------------------------

model <- "bisbas =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20"

# get model fit for baseline model
bisbas_mod_fit <- cfa(model, ordered = c(paste0("x", 1:20)), data = qdat_bisbas_wide)
summary(bisbas_mod_fit, standardized = TRUE, fit.measures = TRUE)

semPaths(bisbas_mod_fit, "std",
         curvePivot = TRUE, thresholds = FALSE)

qdat_bisbas_wide <- qdat_bisbas_wide %>%
  mutate(across(x1:x20, ~ factor(., levels = 1:4)))


# obtain CFI statistics
semTools::measurementInvariance(model = model,
                      data = qdat_bisbas_wide,
                      group = "wave")




