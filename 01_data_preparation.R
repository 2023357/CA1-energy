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
# Find the data folder: the CSV may be inside a "data" subfolder (as in the
# GitHub repository) or next to this script (if files were downloaded
# separately from Moodle). Both layouts work.
data_dir <- if (file.exists(file.path("data", "owid-energy-data.csv"))) "data" else "."
if (!file.exists(file.path(data_dir, "owid-energy-data.csv"))) {
  stop("owid-energy-data.csv not found. Put it in a 'data' folder or in the ",
       "working directory: ", getwd())
}
raw <- read.csv(file.path(data_dir, "owid-energy-data.csv"), stringsAsFactors = FALSE)

cat("\n===== 1. RAW DATASET =====\n")
cat("Rows:", nrow(raw), " Columns:", ncol(raw), "\n")
cat("Years covered:", min(raw$year), "-", max(raw$year), "\n")
cat("Distinct entities:", length(unique(raw$country)), "\n")

# The raw file mixes real countries with aggregates (e.g. "World", "Europe",
# "High-income countries"). Aggregates have no ISO code or an "OWID_" code.
aggregates <- unique(raw$country[raw$iso_code == "" | grepl("^OWID", raw$iso_code)])
cat("Aggregate (non-country) entities found:", length(aggregates), "\n")
cat("Examples:", paste(head(aggregates, 6), collapse = ", "), "\n")

# ---- 2. Scope: countries only, 2000-2022 -----------------------------------
# Justification:
#  * Aggregates double-count countries, so they are removed.
#  * Before 2000 the electricity-mix variables are very sparse.
#  * GDP is 100% missing for 2023-2024 in this release, so 2022 is the last
#    year with all key variables available.
energy <- raw %>%
  filter(iso_code != "", !grepl("^OWID", iso_code),
         year >= 2000, year <= 2022)

cat("\n===== 2. AFTER SCOPING (countries, 2000-2022) =====\n")
cat("Rows:", nrow(energy), " Countries:", length(unique(energy$country)), "\n")

# ---- 3. Attribute selection -------------------------------------------------
# From 130 columns we keep identifiers + variables that describe:
#  - size & wealth (population, GDP), energy use, electricity mix (shares %)
#  - carbon intensity of electricity (gCO2 per kWh), the outcome of interest.
# Many of the 130 columns are redundant transformations of the same quantity
# (e.g. *_consumption, *_cons_change_pct, *_cons_per_capita), so one version
# of each concept is kept to avoid duplicated information.
energy <- energy %>%
  select(country, iso_code, year,
         population, gdp,
         energy_per_capita,               # primary energy use, kWh per person
         electricity_demand_per_capita,   # kWh per person
         carbon_intensity_elec,           # gCO2 per kWh of electricity
         coal_share_elec, gas_share_elec, oil_share_elec,
         nuclear_share_elec, hydro_share_elec,
         wind_share_elec, solar_share_elec,
         renewables_share_elec,           # % of electricity from renewables
         greenhouse_gas_emissions)        # MtCO2e from electricity

cat("\n===== 3. SELECTED ATTRIBUTES =====\n")
str(energy)

# ---- 4. Missing values ------------------------------------------------------
missing_tbl <- energy %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(energy), 1)) %>%
  arrange(desc(pct_missing))

cat("\n===== 4. MISSING VALUES (before cleaning) =====\n")
print(as.data.frame(missing_tbl))

# Plot: % missing per variable
p_missing <- ggplot(filter(missing_tbl, pct_missing > 0),
                    aes(x = reorder(variable, pct_missing), y = pct_missing)) +
  geom_col(fill = "#2a6f97") +
  geom_text(aes(label = paste0(pct_missing, "%")), hjust = -0.1, size = 3.2) +
  coord_flip() +
  expand_limits(y = max(missing_tbl$pct_missing) * 1.15) +
  labs(title = "Missing values per variable (countries, 2000-2022)",
       x = NULL, y = "% of rows missing") +
  theme_minimal(base_size = 12)
ggsave("figures/01_missing_values.png", p_missing, width = 8, height = 5, dpi = 150)

# Strategy:
#  (a) GDP (~25% missing): gaps INSIDE a country's series are filled by linear
#      interpolation between the known years (GDP changes smoothly over time).
#      Countries with no GDP at all are NOT imputed - inventing a whole series
#      would add fake information - so those rows are dropped.
#  (b) Other variables (<8% missing): rows with any remaining NA are removed
#      (complete-case analysis). PCA cannot handle NAs and the loss is small.
interp_inside <- function(y) {
  ok <- which(!is.na(y))
  if (length(ok) < 2) return(y)                # nothing to interpolate
  idx <- seq_along(y)
  inside <- idx >= min(ok) & idx <= max(ok)    # never extrapolate past the ends
  y[inside] <- approx(idx[ok], y[ok], xout = idx[inside])$y
  y
}

gdp_na_before <- sum(is.na(energy$gdp))
energy <- energy %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(gdp = interp_inside(gdp)) %>%
  ungroup()
cat("\nGDP values filled by interpolation:", gdp_na_before - sum(is.na(energy$gdp)), "\n")
# FINDING: zero values were filled -> GDP is never missing in the middle of a
# series. It is missing for WHOLE countries (structurally missing, mostly small
# or data-poor states). Imputation would be fabrication, so these countries
# leave the dataset in step 8. This is a limitation to discuss on the poster:
# the clean data slightly over-represents countries with good statistics.
cat("Countries with NO GDP data at all:",
    sum(tapply(energy$gdp, energy$country, function(g) all(is.na(g)))), "\n")

# ---- 5. Data validity checks -----------------------------------------------
# Shares must lie in [0, 100]; per-capita values and intensities must be >= 0.
share_cols <- grep("_share_elec$", names(energy), value = TRUE)
invalid_shares <- sum(sapply(share_cols, function(v)
  sum(energy[[v]] < 0 | energy[[v]] > 100, na.rm = TRUE)))
cat("\n===== 5. VALIDITY =====\n")
cat("Share values outside [0,100]:", invalid_shares, "\n")
cat("Duplicated country-year rows:", sum(duplicated(energy[, c("country", "year")])), "\n")

# ---- 6. Outliers ------------------------------------------------------------
# Detection with the IQR rule: value > Q3 + 1.5*IQR or < Q1 - 1.5*IQR.
iqr_outliers <- function(x) {
  q <- quantile(x, c(.25, .75), na.rm = TRUE)
  sum(x < q[1] - 1.5 * diff(q) | x > q[2] + 1.5 * diff(q), na.rm = TRUE)
}
num_cols <- names(energy)[sapply(energy, is.numeric) & names(energy) != "year"]
outlier_tbl <- data.frame(variable = num_cols,
                          n_outliers = sapply(energy[num_cols], iqr_outliers),
                          row.names = NULL) %>% arrange(desc(n_outliers))
cat("\n===== 6. IQR OUTLIERS (before treatment) =====\n")
print(outlier_tbl)

# Top extreme cases of energy use per person:
cat("\nHighest energy_per_capita (kWh/person):\n")
print(energy %>% arrange(desc(energy_per_capita)) %>%
        select(country, year, population, energy_per_capita) %>% head(5))

# Treatment decision (critical point for the poster):
#  * Most IQR "outliers" are REAL: Qatar, Iceland, Norway really use a lot of
#    energy; Paraguay/Norway really have ~100% hydro. Removing them would
#    delete the most interesting cases, so they are KEPT.
#  * Very small territories (population < 100,000) are STRUCTURAL outliers:
#    one refinery or power plant dominates the statistics (e.g. the US Virgin
#    Islands, ~650,000 kWh/person because of the HOVENSA oil refinery). They
#    are not comparable to national energy systems, so they are REMOVED.
#  * Skewed variables (population, GDP, energy use) will be log-transformed
#    for plots and PCA instead of being trimmed.
before_out <- energy                                 # copy for the plot
energy <- energy %>% filter(population >= 1e5)

# ---- 7. Feature engineering -------------------------------------------------
energy <- energy %>%
  mutate(
    gdp_per_capita = gdp / population,                       # US$ (2011 int.$)
    fossil_share_elec = coal_share_elec + gas_share_elec + oil_share_elec,
    ghg_per_capita = greenhouse_gas_emissions * 1e6 / population,  # tCO2e/person
    # Categorical 1 (nominal): the largest source of electricity in that year
    dominant_source = c("Coal", "Gas", "Oil", "Nuclear", "Hydro", "Wind", "Solar")[
      max.col(cbind(coal_share_elec, gas_share_elec, oil_share_elec,
                    nuclear_share_elec, hydro_share_elec,
                    wind_share_elec, solar_share_elec), ties.method = "first")],
    # Categorical 2 (ordinal): time period
    period = cut(year, breaks = c(1999, 2007, 2015, 2022),
                 labels = c("2000-2007", "2008-2015", "2016-2022"))
  ) %>%
  select(-gdp, -greenhouse_gas_emissions)          # replaced by per-capita versions

# ---- 8. Final complete-case filter -----------------------------------------
n_before <- nrow(energy)
energy <- energy %>% filter(complete.cases(.))
cat("\n===== 8. COMPLETE CASES =====\n")
cat("Rows removed because of remaining NAs:", n_before - nrow(energy), "\n")

# Categorical 3 (ordinal): energy-use level, quartiles of energy_per_capita.
# Created AFTER cleaning so the quartiles describe the final data.
energy <- energy %>%
  mutate(energy_use_level = cut(energy_per_capita,
                                breaks = quantile(energy_per_capita, 0:4 / 4),
                                labels = c("Low", "Medium", "High", "Very high"),
                                include.lowest = TRUE))



# ---- 9. Outlier plot: before vs after ---------------------------------------
cmp <- bind_rows(
  before_out %>% transmute(stage = "Before (all territories)", energy_per_capita),
  energy     %>% transmute(stage = "After (population >= 100k)", energy_per_capita)) %>%
  mutate(stage = factor(stage, levels = c("Before (all territories)",
                                          "After (population >= 100k)"))) %>%
  filter(energy_per_capita > 0)   # log scale cannot show 0 / NA values
# Label the most extreme entity in each panel so the reader sees WHO it is
extremes <- bind_rows(
  before_out %>% slice_max(energy_per_capita, n = 1) %>%
    mutate(stage = "Before (all territories)"),
  energy %>% slice_max(energy_per_capita, n = 1) %>%
    mutate(stage = "After (population >= 100k)")) %>%
  mutate(stage = factor(stage, levels = levels(cmp$stage)),
         label = paste0(country, " ", year, "\n",
                        scales::comma(round(energy_per_capita)), " kWh"))

p_out <- ggplot(cmp, aes(x = stage, y = energy_per_capita)) +
  geom_boxplot(fill = "#a9d6e5", outlier.colour = "#c1121f", outlier.alpha = .4) +
  geom_text(data = extremes, aes(label = label), hjust = -0.15, size = 3.3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Energy use per person: effect of removing tiny territories",
       subtitle = "Red points = IQR outliers. Remaining outliers (Qatar, Iceland...) are real and kept.",
       x = NULL, y = "kWh per person") +
  theme_minimal(base_size = 12)
ggsave("figures/01_outliers_before_after.png", p_out, width = 8, height = 5, dpi = 150)

# ---- 10. Final characterisation --------------------------------------------
cat("\n===== 10. CLEAN DATASET =====\n")
cat("Rows:", nrow(energy), " Columns:", ncol(energy), "\n")
cat("Countries:", length(unique(energy$country)),
    " Years:", min(energy$year), "-", max(energy$year), "\n")
cat("Missing values left:", sum(is.na(energy)), "\n\n")
print(summary(select(energy, where(is.numeric), -year)))
cat("\nDominant source counts:\n");  print(table(energy$dominant_source))
cat("\nEnergy-use level counts:\n"); print(table(energy$energy_use_level))

# Saved in the same folder as the raw data, so script 02 can find it
write.csv(energy, file.path(data_dir, "energy_clean.csv"), row.names = FALSE)
cat("\nSaved:", file.path(data_dir, "energy_clean.csv"), "\n")

print(p_missing)   # valores ausentes
print(p_out)       # outliers antes e depois

