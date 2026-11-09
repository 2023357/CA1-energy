# =============================================================================
# CA1 - Data Exploration & Preparation
# Script 01: Data understanding, characterisation and preparation
# Domain: ENERGY
#
# Research problem:
#   "What characterises the transition of national electricity systems away
#    from fossil fuels (2000-2022), and how is it related to wealth and
#    energy use?"
#
# Dataset: Our World in Data (OWID) - Energy dataset
#   Source: https://github.com/owid/energy-data (owid-energy-data.csv)
#
# Output of this script:
#   - data/energy_clean.csv        (clean dataset used by scripts 02 and 03)
#   - figures/01_missing_values.png
#   - figures/01_outliers_before_after.png
#   - console output with all characterisation tables
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------
# dplyr  -> data manipulation (filter, mutate, group_by, summarise)
# tidyr  -> reshaping data (pivot_longer) for plots
# ggplot2 -> visualisations
library(dplyr)
library(tidyr)
library(ggplot2)

dir.create("figures", showWarnings = FALSE)   # folder for all saved plots
set.seed(2026)                                 # reproducibility

# ---- 1. Load the raw data ---------------------------------------------------
raw <- read.csv("data/owid-energy-data.csv", stringsAsFactors = FALSE)

cat("\n===== 1. RAW DATASET =====\n")
cat("Rows:", nrow(raw), " Columns:", ncol(raw), "\n")
cat("Years covered:", min(raw$year), "-", max(raw$year), "\n")
cat("Distinct entities:", length(unique(raw$country)), "\n")

# The raw file mixes real countries with aggregates (e.g. "World", "Europe",
# "High-income countries"). Aggregates have no ISO code or an "OWID_" code.
aggregates <- unique(raw$country[raw$iso_code == "" | grepl("^OWID", raw$iso_code)])
cat("Aggregate (non-country) entities found:", length(aggregates), "\n")
cat("Examples:", paste(head(aggregates, 6), collapse = ", "), "\n")

