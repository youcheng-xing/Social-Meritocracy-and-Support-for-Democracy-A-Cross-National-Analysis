# ==============================================================================
# PROJECT: Meritocracy Beliefs and Preferences for Democratic Regimes
# SCRIPT:  01_data_cleaning.R
# AUTHOR:  Oscar Xing (University of Chicago, MAPSS)
# DATE:    February 12, 2025 (Original); Updated April 2026
# ------------------------------------------------------------------------------
# DESCRIPTION: 
#   This script imports raw survey data from the World Values Survey (WVS) 
#   and the EIU Democracy Index. It performs data tidying, variable recoding, 
#   and merges datasets at the individual and country levels.
#
# INPUTS:
#   - data/raw/WVS_Wave7.csv
#   - data/raw/WVS_Time_Series_1981-2022_csv_v5_0.csv
#   - data/raw/democracy-index-eiu.csv
#
# OUTPUTS:
#   - data/processed/merit_times.rds
#   - data/processed/merit_times_country.rds
#   - data/processed/merit7.rds
#   - data/processed/merit7_country.rds
#   - data/processed/merit_scaled.rds
#   - data/processed/merit_scaled_times.rds
#   - data/processed/merit7_scaled_country.rds
#   - data/processed/merit_scaled_country_times.rds
# ==============================================================================

# 1. Setup ----

library(tidyverse)
library(here)

# 2. Functions ----

# 2.1 Function to classify regime type based on EIU Democracy Score
classify_regime <- function(score) {
  regime <- case_when(
    score < 4 ~ "authoritarian regime",
    score < 6 ~ "hybrid regime",
    score < 8 ~ "flawed democracy",
    score >= 8 ~ "full democracy",
    TRUE ~ NA_character_
  )
  # Return as an ordered factor
  factor(regime, levels = c("authoritarian regime", "hybrid regime", "flawed democracy", "full democracy"))
}

# 2.2 Function to categorize meritocracy scores (recoded) into Low/Medium/High levels
categorize_merit <- function(score) {
  level <- case_when(
    score <= 3 ~ "Low",
    score <= 7 ~ "Medium",
    score > 7  ~ "High",
    TRUE ~ NA_character_
  )
  factor(level, levels = c("Low", "Medium", "High"))
}

# 2.3 Function to standardize numeric variables and output as a data frame
scale_numeric_df <- function(data) {
  numeric_data <- data[sapply(data, is.numeric)]
  scaled_df <- numeric_data %>%
    scale() %>%
    as.data.frame()
  
  return(scaled_df)
}

# 3. Load data ----

# World Values Survey Wave 7
# Source: Haerpfer, C., et al. (2022). Version 5.0.0.
wvs7 <- read_csv(here("data","raw","WVS_Wave7.csv"),
                 show_col_types = FALSE
                 )

# EIU Democracy Index
# Source: Economist Intelligence Unit
demo_index <- read_csv(here("data","raw","democracy-index-eiu.csv"),
                       show_col_types = FALSE
                       )
# World Values Survey Time Series
# Source: WVS Association. Version 5.0.0.
wvs_times <- read_csv(here("data","raw","WVS_Time_Series_1981-2022_csv_v5_0.csv"),
                      show_col_types = FALSE
                      )

# 4. Clean the data ----

# 4.1 WVS Time Series: Individual-Level Cleaning
# Select variables and perform initial cleaning
merit_times <- wvs_times %>%
  filter(S001 == 2) %>%
  select(
    wave = S002VS,
    ind_id = S007,
    country_code = COUNTRY_ALPHA,
    merit = E040, # Hard work vs. Luck
    demo_prefer = E117 # Preference for democracy
  ) %>%
  # Create Categorical Meritocracy Level
  mutate(
    meritocracy_level = case_when(
      merit >= 1 & merit <= 3 ~ "High",
      merit >= 4 & merit <= 7 ~ "Medium",
      merit >= 8 & merit <= 10 ~ "Low",
      TRUE ~ NA_character_
    ),
    meritocracy_level = factor(meritocracy_level, levels = c("Low", "Medium", "High")),
    # Recode: 4 = High preference for democracy (Reverse Coded)
    demo_prefer = ifelse(demo_prefer %in% 1:4, 5 - demo_prefer, NA_real_),
    # Recode: 10 = High belief in meritocracy (Reverse Coded)
    meritocracy = ifelse(merit %in% 1:10, 11 - merit, NA_real_)
  ) %>%
  drop_na() %>%
  select(
    ind_id, wave, country_code, meritocracy, meritocracy_level, demo_prefer
  )

# Merge with Democracy Index
demo_index_times <- demo_index %>%
  group_by(Code) %>%
  summarise(demo_score = mean(`Democracy score`)) %>% #use the mean value
  select(country_code = `Code`,
         demo_score) %>%
  drop_na()

merit_times <- merit_times %>%
  mutate(country_code = as.character(country_code)) %>% 
  left_join(demo_index_times, by = "country_code") %>%
  drop_na(demo_score)  %>%
# Categorize regime type by the democracy score
  mutate(regime_type = classify_regime(demo_score))

# Save the processed time-series dataset
saveRDS(merit_times, here("data", "processed", "merit_times.rds"))

# 4.2 Aggregate time-series to country-wave level
# Note: We aggregate by both country and wave to capture temporal variation
merit_times_country<- merit_times %>%
   group_by(country_code, wave) %>%
  summarise(
    meritocracy_country = mean(meritocracy, na.rm = TRUE),
    demo_prefer_country = mean(demo_prefer, na.rm = TRUE),
    demo_score = mean(demo_score, na.rm = TRUE),
    .group = "drop"
    ) %>%
  # Classification of Regime Types
  mutate(
    regime_type = classify_regime(demo_score),
  # Classification of Aggregate Meritocracy Levels
    meritocracy_level = categorize_merit(meritocracy_country)
    )
# Save the country-level panel data
saveRDS(merit_times_country, here("data", "processed", "merit_times_country.rds"))

# 4.3 WVS Wave 7: Individual-Level Cleaning

merit7 <- wvs7 %>%
  select(
    ind_id = D_INTERVIEW, 
    country_code = B_COUNTRY_ALPHA, 
    Q110, Q238, Q260, 
    age = Q262, 
    income = Q288R, 
    Q287, 
    edu = Q275
  ) %>%
  # Create Categorical Meritocracy Levels
  mutate(
    meritocracy_level = case_when(
      Q110 >= 1 & Q110 <= 3 ~ "High",
      Q110 >= 4 & Q110 <= 7 ~ "Medium",
      Q110 >= 8 & Q110 <= 10 ~ "Low",
      TRUE ~ NA_character_
    ),
    meritocracy_level = factor(meritocracy_level, levels = c("Low", "Medium", "High")),
    # Recode: 4 = High preference for democracy (Reverse Coded)
    demo_prefer = ifelse(Q238 %in% 1:4, 5 - Q238, NA_real_), 
    # Recode: 10 = High belief in meritocracy (Reverse Coded)
    meritocracy = ifelse(Q110 %in% 1:10, 11 - Q110, NA_real_), 

    sex = case_when(
      Q260 == 1 ~ 1,
      Q260 == 2 ~ 0,
      TRUE ~ NA_real_
    ), # male = 1, female = 0
    # Recode: 5 = High social class (Reverse Coded)   
    class = ifelse(Q287 > 0, 6 - Q287, NA_real_) 
  ) %>%
  filter(
    age >= 0,
    income > 0,
    edu > 0
  ) %>%
  drop_na() %>%
  select(
    ind_id, country_code, age, income, edu, sex, 
    meritocracy, meritocracy_level, demo_prefer, class
  )

# Merge with Democracy Index (2022)
demo_index7 <- demo_index %>%
  filter(Year == 2022) %>%
  select(country_code = `Code`,
         country_name = `Entity`,
         demo_score = `Democracy score`) %>%
  drop_na()

merit7 <- merit7 %>%
  mutate(country_code = as.character(country_code)) %>% 
  left_join(demo_index7, by = "country_code") %>%
  drop_na(demo_score) %>%
# Categorize regime type by the democracy score
  mutate(regime_type = classify_regime(demo_score))

# Save the processed Wave 7 dataset
saveRDS(merit7, here("data", "processed", "merit7.rds"))

# 4.4 Aggregate Wave 7 dataset to country level

# Note: We collapse the individual-level data to analyze cross-national patterns

merit7_country <- merit7 %>%
  group_by(country_code) %>%
  summarise(
    income_country = mean(income, na.rm = TRUE),
    edu_country = mean(edu, na.rm = TRUE),
    meritocracy_country = mean(meritocracy, na.rm = TRUE),
    demo_prefer_country = mean(demo_prefer, na.rm = TRUE),
    class_country = mean(class, na.rm = TRUE),
    demo_score = mean(demo_score, na.rm = TRUE),
    .groups = "drop"
    ) %>%
  mutate(
    regime_type = classify_regime(demo_score),
    meritocracy_level = categorize_merit(meritocracy_country))

# Save the country-level Wave 7 dataset
saveRDS(merit7_country, here("data", "processed", "merit7_country.rds"))

# 4.5 Standardization

# Wave 7 Standardization at the individual level
merit_scaled <- scale_numeric_df(merit7)
saveRDS(merit_scaled, here("data", "processed", "merit_scaled.rds"))

# Panel Standardization at the individual level
merit_scaled_times <- scale_numeric_df(merit_times)
saveRDS(merit_scaled_times, here("data", "processed", "merit_scaled_times.rds"))

# Wave 7 Standardization at the country level
merit7_scaled_country <- scale_numeric_df(merit7_country)
saveRDS(merit7_scaled_country, here("data", "processed", "merit7_scaled_country.rds"))

# Panel Standardization at the country level
merit_scaled_country_times <- scale_numeric_df(merit_times_country)
saveRDS(merit_scaled_country_times, here("data", "processed", "merit_scaled_country_times.rds"))