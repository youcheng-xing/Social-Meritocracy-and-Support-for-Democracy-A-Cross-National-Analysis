# ==============================================================================
# PROJECT: Meritocracy Beliefs and Preferences for Democratic Regimes
# SCRIPT:  02_analysis.R
# AUTHOR:  Oscar Xing (University of Chicago, MAPSS)
# DATE:    February 12, 2025 (Original); Updated April 2026
# ------------------------------------------------------------------------------
# DESCRIPTION: 
#   This script executes the core statistical analysis of the project. It covers:
#   1. Cross-sectional OLS regressions using WVS Wave 7 (Individual & Country).
#   2. Panel data analysis using WVS Time-Series (Fixed Effects models).
#   3. Sensitivity analysis for causal inference (Sensemakr).
#   4. Heterogeneity analysis (Interactions with Democracy scores and Regimes).
#
# INPUTS (from data/processed/):
#   - merit7.rds, merit_scaled.rds (Individual Wave 7)
#   - merit_times.rds, merit_scaled_times.rds (Individual Time-Series)
#   - merit7_country.rds, merit7_scaled_country.rds (Country Wave 7)
#   - merit_times_country.rds, merit_scaled_country_times.rds (Country Time-Series)
#
# OUTPUTS (to data/model/):
#   - ols_wave7_models.rds      : Main OLS results for Table 3
#   - ols_ts_models.rds         : Time-series/FE results for Table 4
#   - sensitivity_ind.rds       : Sensitivity analysis objects
#   - sensitivity_benchmarks.rds: Partial R2 data for benchmarking
#   - sensitivity_country.rds.  : Sensitivity analysis at Country level
#   - hetero_demo_models.rds    : Interaction with Democracy scores
#   - hetero_regime_models.rds  : Interaction with Regime types
# ==============================================================================


# 1. Setup for data analysis ----
library(tidyverse)
library(estimatr)
library(sensemakr)
library(fixest)
library(here)

# 2. Load the processed data ----

# Wave 7 cross-sectional data
merit7 <- readRDS(here("data", "processed", "merit7.rds"))
# Standardized Wave 7 data
merit_scaled <- readRDS(here("data", "processed", "merit_scaled.rds"))
# Time-series (Panel) data
merit_times <- readRDS(here("data", "processed", "merit_times.rds"))
# Standardized time-series data
merit_scaled_times <- readRDS(here("data", "processed", "merit_scaled_times.rds"))

# Wave 7 country-level
merit7_country <- readRDS(here("data", "processed", "merit7_country.rds"))
# Standardized Wave 7 country-level data
merit7_scaled_country <- readRDS(here("data", "processed", "merit7_scaled_country.rds"))
# Time-series country-wave panel
merit_times_country <- readRDS(here("data", "processed", "merit_times_country.rds"))
# Standardized country-wave panel data
merit_scaled_country_times <- readRDS(here("data", "processed", "merit_scaled_country_times.rds"))

# 3. OLS Regression ----

# 3.1 Meritocracy and Democratic Preference (Wave 7)
# Note: This table combines individual and country-level models, raw and scaled.

# ---Individual Level ---
# Bivariate
t3_m1 <- lm_robust(demo_prefer ~ meritocracy, data = merit7)
# Raw
t3_m2 <- lm_robust(
  demo_prefer ~ meritocracy + age + sex + income + class + edu + demo_score,
  data = merit7
  )
# Scaled
t3_m3 <- lm_robust(
  demo_prefer ~ meritocracy + age + sex + income + class + edu + demo_score, 
  data = merit_scaled
  )
# ---Country Level ---
# Bivariate
t3_m4 <- lm_robust(demo_prefer_country ~ meritocracy_country, 
                   data = merit7_country
                   )
# Raw
t3_m5 <- lm_robust(demo_prefer_country ~ meritocracy_country + demo_score, 
                   data = merit7_country
                   )
# Scaled
t3_m6 <- lm_robust(demo_prefer_country ~ meritocracy_country + demo_score, 
                   data = merit7_scaled_country
                   )

ols_wave7_models <- list(
  "(1)" = t3_m1,
  "(2)" = t3_m2,
  "(3)" = t3_m3,
  "(4)" = t3_m4,
  "(5)" = t3_m5,
  "(6)" = t3_m6
  )
saveRDS(ols_wave7_models, here("data", "model","ols_wave7_models.rds"))

# 3.2 Meritocracy and Democratic Preference (Time Series)
t4_m1 <- lm_robust(demo_prefer ~ meritocracy, data = merit_times)
t4_m2 <- lm_robust(demo_prefer ~ meritocracy + demo_score, data = merit_times)
t4_m3 <- feols(
  demo_prefer ~ meritocracy | country_code + wave,
  data = merit_times
)
t4_m4 <- lm_robust(demo_prefer_country ~ meritocracy_country, 
                   data = merit_times_country
                   )
t4_m5 <- lm_robust(demo_prefer_country ~ meritocracy_country + demo_score, 
                   data = merit_times_country
                   )
t4_m6 <- feols(
  demo_prefer_country ~ meritocracy_country | country_code + wave,
  data = merit_times_country
  )

ols_ts_models <- list(
  "(1)" = t4_m1,
  "(2)" = t4_m2,
  "(3)" = t4_m3,
  "(4)" = t4_m4,
  "(5)" = t4_m5,
  "(6)" = t4_m6
  )

saveRDS(ols_ts_models, here("data", "model", "ols_ts_models.rds"))

# 4. Sensitivity Analysis ----

# 4.1 Individual level
# Baseline model for sensitivity
sens_ind <- lm(demo_prefer ~ meritocracy + age + sex + income + class + edu + demo_score, data = merit7)

# Calculate partial R2 for benchmarking
partial_r2_treat <- partial_r2(sens_ind, treatment = "meritocracy")
partial_r2_outcome <- partial_r2(sens_ind, outcome = "demo_prefer")

benchmarks_df <- 
  data.frame(variable = names(partial_r2_treat),
                           R2_with_treat = partial_r2_treat,
                           R2_with_outcome = partial_r2_outcome) %>%
  mutate(avg_R2 = (R2_with_treat + R2_with_outcome)/2) %>%
  arrange(desc(avg_R2))

# Run the sensemakr analysis
#Choosing 'edu' as benchmark because it shows the strongest partial R2 with the outcome.

sensitivity_ind <- sensemakr(
  model = sens_ind,
  treatment = "meritocracy",
  benchmark_covariates = "edu",
  kd = 1:3
)

saveRDS(benchmarks_df, here("data", "model", "benchmarks_df.rds"))
saveRDS(sensitivity_ind, here("data", "model", "sensitivity_ind.rds"))


# 4.2 Country level
# I did not calculate for benchmarking because there was only one covariate in this model
sens_country <- lm(demo_prefer_country ~ meritocracy_country + demo_score, data = merit_times_country)
sensitivity_country <- sensemakr(
  model = sens_country,
  treatment = "meritocracy_country",
  benchmark_covariates = "demo_score",
  kd = 1:3)

saveRDS(sensitivity_country, here("data", "model", "sensitivity_country.rds"))

# 5. Heterogeneity analysis ----

# 5.1 Heterogeneity by democracy score

t5_m1 <- lm_robust(
  demo_prefer ~ meritocracy + age + sex + income + class + edu + demo_score + 
    meritocracy * demo_score, 
  data = merit7
  )
t5_m2 <- lm_robust(
  demo_prefer_country ~ meritocracy_country + demo_score + 
    meritocracy_country * demo_score, 
  data = merit7_country
  )
t5_m3 <- lm_robust(
  demo_prefer ~ meritocracy + demo_score + meritocracy * demo_score, 
  data = merit_times
  )
t5_m4 <- lm_robust(
  demo_prefer_country ~ meritocracy_country + demo_score + 
    meritocracy_country * demo_score, 
  data = merit_times_country
  )


hetero_demo_models <- list(
  "(1)" = t5_m1,
  "(2)" = t5_m2,
  "(3)" = t5_m3,
  "(4)" = t5_m4
  )

saveRDS(hetero_demo_models, here("data", "model","hetero_demo_models.rds"))


# 5.2 Heterogeneity by regime type

ap_m1 <- lm_robust(demo_prefer ~ meritocracy + age + sex + income + class + edu + I(regime_type) + meritocracy * I(regime_type), data = merit7)
ap_m2 <- lm_robust(demo_prefer_country ~ meritocracy_country + I(regime_type) + meritocracy_country * I(regime_type), data = merit7_country)
ap_m3 <- lm_robust(demo_prefer ~ meritocracy + I(regime_type) + meritocracy * I(regime_type), data = merit_times)
ap_m4 <- lm_robust(demo_prefer_country ~ meritocracy_country + I(regime_type) + meritocracy_country * I(regime_type), data = merit_times_country)

hetero_regime_models <- list(
  "(1)" = ap_m1,
  "(2)" = ap_m2,
  "(3)" = ap_m3,
  "(4)" = ap_m4
  )
saveRDS(hetero_regime_models, here("data", "model","hetero_regime_models.rds"))