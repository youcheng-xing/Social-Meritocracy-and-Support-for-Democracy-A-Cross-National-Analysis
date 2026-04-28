# ==============================================================================
# PROJECT: Meritocracy Beliefs and Preferences for Democratic Regimes
# SCRIPT:  02_analysis.R
# AUTHOR:  Oscar Xing (University of Chicago, MAPSS)
# DATE:    February 12, 2025 (Original); Updated April 2026
# ------------------------------------------------------------------------------
# DESCRIPTION: 
#   This script produces all tables and figures for the manuscript.
#   It reads model objects from data/model/ and clean data from data/processed/.
# ==============================================================================

# 1. Setup ----
library(tidyverse)
library(here)
library(modelsummary) 
library(kableExtra)   
library(ggplot2)      
library(ggrepel)      
library(sensemakr)   
library(sf)           
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(scales)

options(knitr.table.format = "latex")
options(kableExtra.auto_format = FALSE)


# 2. Load data and models ----

# Datasets for plotting
merit7 <- readRDS(here("data", "processed", "merit7.rds"))
merit_times <- readRDS(here("data", "processed", "merit_times.rds"))
merit7_country <- readRDS(here("data", "processed", "merit7_country.rds"))
merit_times_country <- readRDS(here("data", "processed", "merit_times_country.rds"))

# Models
ols_wave7_models <- readRDS(here("data", "model", "ols_wave7_models.rds"))
ols_ts_models <- readRDS(here("data", "model", "ols_ts_models.rds"))
hetero_demo_models <- readRDS(here("data", "model", "hetero_demo_models.rds"))
hetero_regime_models <- readRDS(here("data", "model", "hetero_regime_models.rds"))
benchmarks_df <- readRDS(here("data", "model", "benchmarks_df.rds"))
sensitivity_ind <- readRDS(here("data", "model", "sensitivity_ind.rds"))
sensitivity_country <- readRDS(here("data", "model", "sensitivity_country.rds"))

# 3. Global Labels ----
# Define a consistent coefficient map for all tables and plots
coef_names <- c(
  "meritocracy"         = "Social Meritocracy (Individual)",
  "meritocracy_country" = "Social Meritocracy (Country)",
  "demo_prefer"         = "Support for Democracy (Individual)",
  "demo_prefer_country" = "Support for Democracy (Country)",
  
  "age"                 = "Age",
  "sex"                 = "Gender",
  "income"              = "Income",
  "class"               = "Social Class",
  "edu"                 = "Education",
  "demo_score"          = "Democracy Score",
  "regime_type"         = "Regime Type",
  # --- regime type ---
  "I(regime_type)flawed democracy" = "Flawed Democracy",
  "I(regime_type)full democracy"   = "Full Democracy",
  "I(regime_type)hybrid regime"    = "Hybrid Regime",
  
  # --- interaction terms ---
  "meritocracy:I(regime_type)flawed democracy" = "Meritocracy $\\times$ Flawed Democracy",
  "meritocracy:I(regime_type)full democracy"   = "Meritocracy $\\times$ Full Democracy",
  "meritocracy:I(regime_type)hybrid regime"    = "Meritocracy $\\times$ Hybrid Regime",
  
  "meritocracy_country:I(regime_type)flawed democracy" = "Meritocracy $\\times$ Flawed Democracy",
  "meritocracy_country:I(regime_type)full democracy"   = "Meritocracy $\\times$ Full Democracy",
  "meritocracy_country:I(regime_type)hybrid regime"    = "Meritocracy $\\times$ Hybrid Regime",
  
  "meritocracy:demo_score"         = "Meritocracy $\\times$ Democracy Score",
  "meritocracy_country:demo_score" = "Meritocracy $\\times$ Democracy Score"
  
)


# 4. Tables ----

# 4.1 Table 1: Descriptive Statistics of Key Variables ----
table1_data <- merit7 %>%
  summarise(
    across(
      .cols = c(meritocracy, demo_prefer, age, income, class, edu, demo_score),
      .fns = list(
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(Mean|SD|Min|Max)$"
  ) %>%
  mutate(Variable = recode(Variable, !!!coef_names))%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

# Generate LaTex Table
table1_latex <- kable(
  table1_data, 
  format = "latex", 
  booktabs = TRUE,
  linesep = "",
  caption = "Descriptive Statistics of Key Variables",
  col.names = c("Variable", "Mean", "Std. Dev.", "Min", "Max")
) %>%
  footnote(
    general = "Source: World Values Survey (WVS) Wave 7. Estimates based on individual-level unscaled data.",
    general_title = "",
    fixed_small_size = TRUE,
    threeparttable = TRUE
  )%>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE,
    font_size = 10
  )

tex_content1 <- as.character(table1_latex)
tex_content1 <- gsub("&nbsp;", " ", tex_content1)
tex_content1 <- gsub("<", "$<$", tex_content1)

writeLines(tex_content1, here("output", "tables", "table1_descriptive_stats.tex"), useBytes = TRUE)

# 4.2 Table 2: Meritocratic Belief and Support for Democracy by Regime Type ----

table2_data <- merit7 %>%
  filter(!is.na(regime_type)) %>%
  group_by(regime_type) %>%
  summarise(merit_mean = mean(meritocracy, na.rm = TRUE)) %>%
  mutate(regime_type = factor(regime_type, 
                              levels = c("authoritarian regime", "hybrid regime", 
                                         "flawed democracy", "full democracy"))) %>%
  arrange(regime_type) %>%
  mutate(regime_type = str_to_title(regime_type))

# Generate LaTex Table
table2_latex <- table2_data %>%
  kable(
    format = "latex", 
    booktabs = TRUE,
    digits = 2,
    linesep = "",
    caption = "Mean Meritocratic Belief Across Different Regime Types",
    col.names = c("Regime Type", "Mean Meritocratic Belief")
  ) %>%
  footnote(
    general = "Note: Regime classifications are based on the EIU Democracy Index. Values represent group means from WVS Wave 7.",
    general_title = "",
    fixed_small_size = TRUE,
    threeparttable = TRUE
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width = FALSE,
    font_size = 10
  )
  
tex_content2 <- as.character(table2_latex)
tex_content2 <- gsub("&nbsp;", " ", tex_content2)
tex_content2 <- gsub("<", "$<$", tex_content2)

writeLines(tex_content2, here("output", "tables", "table2_regime_summary.tex"), useBytes = TRUE)

# 4.3 Table 3:Effects of Social Meritocracy on Support for Democracy ----
table3_obj <- modelsummary(
  ols_wave7_models,
  output = "kableExtra",
  format = "latex",
  coef_map = coef_names,
  statistic = "std.error",
  stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "r.squared"),
  align = "lcccccc",
  title = "Effects of Social Meritocracy on Support for Democracy",
  booktabs = TRUE,
  escape = FALSE,
  notes = NULL
) %>%
  add_header_above(c(" " = 1, "Bivariate" = 1, "Unscaled" = 1, "Scaled" = 1, 
                     "Bivariate " = 1, "Unscaled " = 1, "Scaled " = 1)) %>%
  add_header_above(c(" " = 1, "Individual-Level Analysis" = 3, "Country-Level Analysis" = 3)) %>%
  footnote(
    general = paste0(
      "Note: * p < 0.1, ** p < 0.05, *** p < 0.01. ",
      "The dependent variable is individual-level support for democracy ",
      "(models 1--3) or country-level averages (models 4--6), measured ",
      "on a four-point scale from the WVS7 dataset. Higher values indicate ",
      "greater support. Entries are linear regression coefficients with ", 
      "heteroskedasticity-robust standard errors in parentheses."
    ),
    general_title = "",
    threeparttable = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE,
                font_size = 7)

tex_content3 <- as.character(table3_obj)
tex_content3 <- gsub("&nbsp;", " ", tex_content3)
tex_content3 <- gsub("<", "$<$", tex_content3)

writeLines(tex_content3, here("output", "tables", "table3_main_results.tex"), useBytes = TRUE)

# 4.4 Table 4: Effects of Social Meritocracy on Support for Democracy using Time Series ----
fe_labels <- c("No", "No", "Country, Wave", "No", "No", "Country, Wave")

fe_info <- data.frame(
  term = "Fixed Effects"
)

for (i in 1:6) {
  fe_info[paste0("(", i, ")")] <- fe_labels[i]
}

table4_obj <- modelsummary(
  ols_ts_models,
  output = "kableExtra",
  format = "latex",
  coef_map = coef_names, 
  statistic = "std.error",
  stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "r.squared"),
  align = "lcccccc",
  add_rows = fe_info,
  
  title = "Effects of Social Meritocracy on Support for Democracy (Panel Data)",
  booktabs = TRUE,
  escape = FALSE,
  notes = NULL
) %>%
  add_header_above(c(" " = 1, "Individual-Level (Time Series)" = 3, "Country-Level (Time Series)" = 3)
  ) %>%
  row_spec(row = 9, bold = TRUE) %>%
  footnote(
    general = paste0(
      "Note: * p < 0.1, ** p < 0.05, *** p < 0.01. ",
      "The dependent variable is individual-level support for democracy ",
      "(model 1-3) or country-level averages (model 4-6), measured on a ", 
      "four-point scale from the WVS Time Series dataset. Higher values ",
      "indicate greater support. Entries are linear regression coefficients ", 
      "with heteroskedasticity-robust standard errors in parentheses."
    ),
    general_title = "",
    threeparttable = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE,
                font_size = 7)

tex_content4 <- as.character(table4_obj)
tex_content4 <- gsub("&nbsp;", " ", tex_content4)
tex_content4 <- gsub("<", "$<$", tex_content4)

writeLines(tex_content4, here("output", "tables", "table4_timeseries_results.tex"), useBytes = TRUE)

# 4.5 Table 5: Interaction Effects of Meritocracy by Democracy Score ----

custom_stars <- c('+' = 0.1, '*' = 0.05, '**' = 0.01, '***' = 0.001)

table5_obj <- modelsummary(
  hetero_demo_models,
  output = "kableExtra",
  format = "latex",
  coef_map = coef_names,
  statistic = "std.error",
  stars = custom_stars,
  gof_map = c("nobs", "r.squared"),
  align = "lcccc",
  title = "Interaction Effects of Meritocracy by Democracy Score",
  booktabs = TRUE,
  escape = FALSE,
  notes = NULL
) %>%
  add_header_above(c(" " = 1, "Individual" = 1, "Country" = 1, "Individual " = 1, "Country " = 1)) %>%
  add_header_above(c(" " = 1, "WVS7" = 2, "WVS Time Series" = 2)) %>%
  footnote(
    general = paste0(
      "Note: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. ",
      "The dependent variable varies by model specification: in models 1 ", 
      "and 3, it is individuals' support for democracy; in models 2 ", 
      "and 4, it is the country-level average. Coefficients are from ", 
      "linear regressions with robust standard errors in parentheses."
    ),
    general_title = "",
    threeparttable = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE,
                font_size = 10)

tex_content5 <- as.character(table5_obj)
tex_content5 <- gsub("&nbsp;", " ", tex_content5)
tex_content5 <- gsub("<", "$<$", tex_content5)

writeLines(tex_content5, here("output", "tables", "table5_interaction_demo.tex"), useBytes = TRUE)


# 4.6 Table 9: Interaction Effects of Meritocracy by Regime Type ----

table9_obj <- modelsummary(
  hetero_regime_models,
  output = "kableExtra",
  format = "latex",
  coef_map = coef_names,
  statistic = "std.error",
  stars = custom_stars,
  gof_map = c("nobs", "r.squared"),
  align = "lcccc",
  title = "Interaction Effects of Meritocracy by Regime Type",
  booktabs = TRUE,
  escape = FALSE,
  notes = NULL
) %>%
  add_header_above(c(" " = 1, "Individual" = 1, "Country" = 1, "Individual " = 1, "Country " = 1)) %>%
  add_header_above(c(" " = 1, "WVS7" = 2, "WVS Time Series" = 2)) %>%
  footnote(
    general = paste0(
      "Note: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001. ",
      "The dependent variable varies by model specification: in ", 
      "models 1 and 3, it is individuals' support for democracy; in ", 
      "models 2 and 4, it is the country-level average. Coefficients ", 
      "are from linear regressions with robust standard errors in parentheses."
    ),
    general_title = "",
    threeparttable = TRUE,
    fixed_small_size = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"),
                full_width = FALSE,
                font_size = 10) 

tex_content9 <- as.character(table9_obj)
tex_content9 <- gsub("&nbsp;", " ", tex_content9)
tex_content9 <- gsub("<", "$<$", tex_content9)

writeLines(tex_content9, here("output", "tables", "table9_interaction_regime.tex"), useBytes = TRUE)

# 4.7 Table 6: Partial R2 for Benchmarking Covariates ----

table6_data <- benchmarks_df %>%
  as_tibble() %>% 
  mutate(
    variable = str_replace_all(variable, "_", " "),
    variable = recode(variable, !!!coef_names)
  ) %>%
  select(variable, R2_with_treat, R2_with_outcome, avg_R2) %>% 
  mutate(across(where(is.numeric), ~round(.x, 3)))

table6_latex <- kable(
  table6_data,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  linesep = "",
  row.names = FALSE,
  caption = "Partial $R^2$ for Benchmarking Covariates",
  col.names = c("Covariate", "$R^2$ with Treatment", "$R^2$ with Outcome", "Average $R^2$")
) %>%
  add_header_above(c(" " = 1, "Partial $R^2$" = 2, " " = 1)) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    full_width = FALSE
  ) %>%
  footnote(
    general = "Note: This table reports average partial $R^2$ of each covariate with the treatment and the outcome.",
    general_title = "",
    fixed_small_size = TRUE,
    threeparttable = TRUE
  )

save_kable(table6_latex, here("output", "tables", "table6_benchmarks.tex"))

# 4.8 Table 7: Sensitivity Analysis with Education as Benchmark Covariate ----
summary_table <- sensitivity_ind$bounds
table_sens_summary <- kable(
  summary_table, 
  format = "latex", 
  booktabs = TRUE, 
  linesep = "", 
  row.names = FALSE,
  caption = "Sensitivity Analysis with Education as Benchmark Covariate",
  
  col.names = c("Bound Label", "R2dz.x", "R2yz.dx", "Treatment", 
                "Adjusted Estimate", "Adjusted Se", "Adjusted T", 
                "Adjusted Lower CI", "Adjusted Upper CI")
) %>% 
  
  kable_styling(latex_options = c("hold_position", "scale_down")) %>% 
  
  footnote(
    general = paste0(
      "Note: This table presents the results of a sensitivity analysis at the individual level ",
      "using education as the benchmark covariate. Each row simulates a hypothetical ",
      "confounder with strength equal to 1x, 2x, or 3x the association of education with ",
      "both the treatment (meritocracy) and the outcome (support for democracy). ",
      "R2dz.x and R2yz.dx represent the proportion of variance in the treatment and outcome, ",
      "respectively, explained by the simulated confounder. Adjusted estimates and confidence ",
      "intervals reflect bias-corrected treatment effects under each scenario."
    ),
    general_title = "",
    fixed_small_size = TRUE,
    threeparttable = TRUE
  )

save_kable(table_sens_summary, here("output", "tables", "table7_sensitivity.tex"))

# 4.9 Table 8: Sensitivity Analysis with Democracy Score as Benchmark Covariate ----
summary_table_country <- sensitivity_country$bounds
table8_latex <- kable(
  summary_table_country, 
  format = "latex", 
  booktabs = TRUE, 
  linesep = "", 
  row.names = FALSE,
  caption = "Table 8: Sensitivity Analysis with Democracy Score as Benchmark Covariate",
  label = "sensitivity_country",

  col.names = c("Bound Label", "R2dz.x", "R2yz.dx", "Treatment", 
                "Adjusted Estimate", "Adjusted Se", "Adjusted T", 
                "Adjusted Lower CI", "Adjusted Upper CI")
) %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"),
                full_width = FALSE) %>% 

  footnote(
    general = paste0(
      "Note: This table reports the results of a sensitivity analysis at the country level ",
      "using democracy score as the benchmark covariate. Each row simulates a hypothetical ",
      "confounder with strength equal to 1x, 2x, or 3x the association of democracy score ",
      "with both the treatment (country-level meritocracy belief) and the outcome ",
      "(average support for democracy). R2dz.x and R2yz.dx represent the proportion of variance ",
      "in the treatment and outcome, respectively, that would be explained by the simulated confounder. ",
      "Adjusted estimates and confidence intervals reflect bias-corrected treatment effects under each scenario."
    ),
    general_title = "",
    fixed_small_size = TRUE,
    threeparttable = TRUE 
  )

save_kable(table8_latex, here("output", "tables", "table8_sensitivity_country.tex"))

# 5. Figure ----
# 5.1 Figure 1: Meritocracy, Support for Democracy, and Democracy Score Across Countries ----
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica")
world_merit <- left_join(world, merit7_country, by = c("iso_a3" = "country_code"))

map_theme <- theme_minimal() +
  theme(
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.position = "right",
    axis.title.x = element_text(size = 12, margin = margin(t = 15), face = "plain"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "grey90", size = 0.2)
  )

# (a) Social Meritocratic Belief
p1 <- ggplot(world_merit) +
  geom_sf(aes(fill = meritocracy_country), color = "darkgrey", size = 0.1) +
  scale_fill_distiller(palette = "Spectral", direction = -1, na.value = "grey80") +
  labs(
    fill = "Meritocratic\nBelief Level", 
    x = "(a) Social Meritocratic Belief"
  ) +
  map_theme

# (b) Support for Democracy
p2 <- ggplot(world_merit) +
  geom_sf(aes(fill = demo_prefer_country), color = "darkgrey", size = 0.1) +
  scale_fill_distiller(palette = "Spectral", direction = -1, na.value = "grey80") +
  labs(
    fill = "Support for\nDemocracy", 
    x = "(b) Support for Democracy"
  ) +
  map_theme

# (c) Democracy Score
p3 <- ggplot(world_merit) +
  geom_sf(aes(fill = demo_score), color = "darkgrey", size = 0.1) +
  scale_fill_distiller(palette = "Spectral", direction = -1, na.value = "grey80") +
  labs(
    fill = "Democracy\nScore", 
    x = "(c) Democracy Score"
  ) +
  map_theme

fig1_combined <- p1 + p2 + p3 + 
  plot_layout(ncol = 3)

ggsave(here("output", "figures", "figure1_global_maps.pdf"), 
       plot = fig1_combined, 
       width = 15, height = 4.5, device = "pdf")

# 5.2 Figure 2: Relationship between Individual Meritocratic Belief and Support for Democracy ----
# Note: This plot explicitly uses the Wave 7 cross-sectional dataset (merit7)
heatmap_data <- merit7 %>%
  drop_na(meritocracy, demo_prefer) %>%
  count(meritocracy, demo_prefer)

p_heatmap <- ggplot(heatmap_data, aes(x = meritocracy, y = demo_prefer, fill = n)) +
  geom_tile(color = "white", size = 0.2) + 
  scale_fill_gradient(
    low = "white", 
    high = "steelblue", 
    name = "Frequency",
    labels = scales::comma
  ) +
  labs(
    x = "Individual Meritocratic Belief", 
    y = "Individual Support for Democracy"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 11, margin = margin(t = 10))
  )

ggsave(here("output", "figures", "figure2_heatmap.pdf"), 
       plot = p_heatmap, 
       width = 8, height = 5, device = "pdf")
# 5.3 Figure 3: Linear Relationship between Meritocratic Belief and Support for Democracy ----
# Note: This plot explicitly uses the Wave 7 country-level aggregated dataset.

scatter_data <- merit7_country %>%
  drop_na(meritocracy_country, demo_prefer_country)

p_scatter <- ggplot(scatter_data, aes(x = meritocracy_country, y = demo_prefer_country)) +
  geom_smooth(method = "lm", color = "darkred", fill = "grey70", alpha = 0.5, se = TRUE, size = 0.8) +
  geom_point(color = "steelblue", size = 2.5, alpha = 0.9) +
  geom_text_repel(
    aes(label = country_code), 
    size = 3.2, 
    color = "black",
    box.padding = 0.3,
    point.padding = 0.3,  
    segment.color = "grey50",
    segment.size = 0.3,
    max.overlaps = Inf
  ) +
  labs(
    x = "Meritocratic Belief", 
    y = "Support for Democracy"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    axis.title.x = element_text(size = 11, margin = margin(t = 12)),
    axis.title.y = element_text(size = 11, margin = margin(r = 12)),
    axis.text = element_text(size = 9, color = "grey30"),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )

ggsave(here("output", "figures", "figure3_country_scatter.pdf"), 
       plot = p_scatter, 
       width = 8, height = 5, device = "pdf")

# 5.4 Figure 4: Meritocracy and Support for Democracy by Regime Type ----
plot4_data <- merit7 %>%
  group_by(regime_type) %>%
  summarise(
    merit_mean = mean(meritocracy, na.rm = TRUE),
    demo_mean  = mean(demo_prefer, na.rm = TRUE)
  ) %>%
  drop_na(regime_type)

p_regime <- ggplot(plot4_data, aes(x = merit_mean, y = demo_mean)) +
  geom_point(size = 4, color = "steelblue", alpha = 0.9) +
  geom_text_repel(
    aes(label = regime_type), 
    size = 4.5,
    box.padding = 0.6,    
    point.padding = 0.5,
    segment.color = "grey60", 
    segment.size = 0.3,
    seed = 42 
  ) +
  labs(
    x = "Mean Meritocratic Belief",
    y = "Mean Support for Democracy"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", size = 0.4),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.text = element_text(color = "grey30"),
    plot.margin = margin(t = 15, r = 25, b = 15, l = 15) 
  )

ggsave(here("output", "figures", "figure4_regime_scatter.pdf"), 
       plot = p_regime, 
       width = 7, height = 5, device = "pdf")

# 5.5 Figure 5: Meritocracy-Democracy Relationship among Authoritarian-Leaning Countries ----
# Note: This plot uses the WVS Wave 7 Country-level aggregated data.

plot5_data <- merit7_country %>%
  filter(regime_type %in% c("authoritarian regime", "hybrid regime")) %>%
  drop_na(meritocracy_country, demo_prefer_country)

p_authoritarian <- ggplot(
  plot5_data, aes(x = meritocracy_country, y = demo_prefer_country)) +
  geom_smooth(method = "lm", color = "darkred", fill = "grey70", alpha = 0.5, se = TRUE, size = 0.8) +
  geom_point(color = "steelblue", size = 2.5, alpha = 0.9) +
  geom_text_repel(
    aes(label = country_code), 
    size = 3.2, 
    color = "black",
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey50",
    segment.size = 0.3,
    max.overlaps = Inf,
    seed = 42
  ) +
  labs(
    x = "Meritocratic Belief", 
    y = "Support for Democracy"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    axis.title.x = element_text(size = 11, margin = margin(t = 12)),
    axis.title.y = element_text(size = 11, margin = margin(r = 12)),
    axis.text = element_text(size = 9, color = "grey30"),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )

ggsave(here("output", "figures", "figure5_authoritarian_scatter.pdf"), 
       plot = p_authoritarian, 
       width = 8, height = 5, device = "pdf")

# 5.6 Figure 6: Meritocracy-Democracy Relationship among Democratic-Leaning Countries ----
# Note: This plot uses the WVS Wave 7 Country-level aggregated data.

plot6_data <- merit7_country %>%
  filter(regime_type %in% c("full democracy", "flawed democracy")) %>%
  drop_na(meritocracy_country, demo_prefer_country)

p_democratic <- ggplot(plot6_data, aes(x = meritocracy_country, y = demo_prefer_country)) +
  geom_smooth(method = "lm", color = "darkred", fill = "grey70", alpha = 0.5, se = TRUE, size = 0.8) +
  geom_point(color = "steelblue", size = 2.5, alpha = 0.9) +
  geom_text_repel(
    aes(label = country_code), 
    size = 3.2, 
    color = "black",
    box.padding = 0.3,
    point.padding = 0.3,
    segment.color = "grey50",
    segment.size = 0.3,
    max.overlaps = Inf, 
    seed = 42
  ) +
  labs(
    x = "Meritocratic Belief", 
    y = "Support for Democracy"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    axis.title.x = element_text(size = 11, margin = margin(t = 12)),
    axis.title.y = element_text(size = 11, margin = margin(r = 12)),
    axis.text = element_text(size = 9, color = "grey30"),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )

ggsave(here("output", "figures", "figure6_democratic_scatter.pdf"), 
       plot = p_democratic, 
       width = 8, height = 5, device = "pdf")

# 5.7 Sensitivity Analysis----
# Figure 7: Individual-Level Sensitivity Plot

pdf(file = here("output", "figures", "figure7_sensitivity_contour_ind.pdf"), 
    width = 7, height = 5)
plot(sensitivity_ind)
dev.off()

# Figure 8: Country-Level Sensitivity Plot

pdf(file = here("output", "figures", "figure8_sensitivity_contour_country.pdf"), 
    width = 7, height = 5)
plot(sensitivity_country)
dev.off()

# 5.8 Figure 9: Inconsistency between Support for Democracy and Actual Democracy Score ----
# Dataset used: merit7_country

plot9_data <- merit7_country %>%
  drop_na(demo_score, demo_prefer_country)

p_inconsistency <- ggplot(plot9_data, aes(x = demo_score, y = demo_prefer_country)) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1.2) +
  geom_point(color = "black", size = 1.8, alpha = 0.8) +
  geom_text_repel(
    aes(label = country_code), 
    size = 3.2,
    box.padding = 0.2,
    point.padding = 0.2,
    segment.color = NA,
    max.overlaps = Inf,
    seed = 42
  ) +
  labs(
    x = "Democracy Score (Objective)", 
    y = "Average Support for Democracy (Subjective)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    axis.title.x = element_text(size = 11, margin = margin(t = 12)),
    axis.title.y = element_text(size = 11, margin = margin(r = 12)),
    axis.text = element_text(size = 9, color = "grey30"),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )

ggsave(here("output", "figures", "figure9_inconsistency_scatter.pdf"), 
       plot = p_inconsistency, 
       width = 8, height = 5, device = "pdf")

