# ==============================================================================
# MASTER REPLICATION SCRIPT: Social Meritocracy and Democracy
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, here, modelsummary, kableExtra, ggplot2, 
  ggrepel, sensemakr, sf, rnaturalearth, rnaturalearthdata, 
  patchwork, scales
)

cat("\n>>> Step 1/3: Data Cleaning and Merging...\n")
source(here("code", "01_data_cleaning.R"), encoding = "UTF-8")

cat("\n>>> Step 2/3: Running Regression Models & Sensitivity Analysis...\n")
source(here("code", "02_analysis.R"), encoding = "UTF-8")

cat("\n>>> Step 3/3: Generating Tables and Figures...\n")
source(here("code", "03_visualization.R"), encoding = "UTF-8")


cat("\n========================================================\n")
cat("SUCCESS: The entire research pipeline has finished!\n")
cat("All outputs are saved in the /output/ directory.\n")
cat("========================================================\n")