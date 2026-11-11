# =============================================================================
# CA1 - Data Exploration & Preparation
# Script 03: Encoding of categorical variables and PCA
# Domain: ENERGY
#
# Input : data/energy_clean.csv   (created by 01_data_preparation.R)
# Output: data/energy_encoded.csv, data/pca_scores.csv
#         figures/03_*.png        (4 plots for the poster)
#
# Part A - Encoding (Lecture 5 / Tutorial 5: dummy coding)
#   dominant_source  (nominal, 6 levels) -> ONE-HOT encoding (fastDummies)
#   period           (ordinal, 3 levels) -> LABEL encoding 1, 2, 3
#   energy_use_level (ordinal, 4 levels) -> LABEL encoding 1, 2, 3, 4
#
# Part B - PCA (dimensionality reduction)
#   13 numeric features -> z-score standardisation (Tutorial 3) -> prcomp()
#   -> choose number of components -> interpret loadings -> link to EDA
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------
# install.packages("fastDummies")   # run once if not installed
library(dplyr)
library(tidyr)
library(ggplot2)
library(fastDummies)   # dummy_cols() for one-hot encoding (as in Tutorial 5)

dir.create("figures", showWarnings = FALSE)

# Same folder logic as script 02: works with or without a "data" subfolder
data_dir <- if (file.exists(file.path("data", "energy_clean.csv"))) "data" else "."
clean_file <- file.path(data_dir, "energy_clean.csv")
if (!file.exists(clean_file)) {
  stop("energy_clean.csv not found. Run 01_data_preparation.R first, ",
       "or put energy_clean.csv in the working directory: ", getwd())
}
energy <- read.csv(clean_file, stringsAsFactors = FALSE)

source_cols <- c(Coal = "#4a4a4a", Oil = "#8c564b", Gas = "#e07b39",
                 Nuclear = "#9467bd", Hydro = "#1f77b4", Wind = "#2ca02c")
theme_poster <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")


# =============================================================================
# PART A - ENCODING
# =============================================================================

# ---- A1. Categorical variables before encoding ------------------------------
cat("\n===== A1. Categorical variables =====\n")
cat("dominant_source:\n");  print(table(energy$dominant_source))
cat("period:\n");           print(table(energy$period))
cat("energy_use_level:\n"); print(table(energy$energy_use_level))

# ---- A2. One-hot encoding of dominant_source (NOMINAL) ----------------------
# Why one-hot: the sources have NO natural order. Label encoding
# (Coal = 1, Gas = 2, Hydro = 3 ...) would wrongly tell an algorithm that
# Hydro is "3 times" Coal, or that Gas lies "between" Coal and Hydro.
# One-hot creates one 0/1 column per category instead.
encoded <- dummy_cols(energy,
                      select_columns = "dominant_source",
                      remove_first_dummy = FALSE,       # keep all 6 (see note)
                      remove_selected_columns = FALSE)  # keep the original too
# Note: for a regression model we would drop one dummy (remove_first_dummy =
# TRUE) to avoid the "dummy variable trap" (perfect multicollinearity).
# Here the dummies are used for description, so all 6 are kept.

dummy_names <- grep("^dominant_source_", names(encoded), value = TRUE)
cat("\n===== A2. One-hot columns created =====\n")
print(dummy_names)
print(head(encoded[, c("country", "year", "dominant_source", dummy_names)], 5))
# Check: every row must have exactly one 1 across the dummy columns
cat("All rows have exactly one dummy = 1:",
    all(rowSums(encoded[, dummy_names]) == 1), "\n")

# ---- A3. Label encoding of ORDINAL variables --------------------------------
# Why label encoding: these categories DO have a natural order, so integers
# keep that order (2016-2022 comes after 2000-2007; "High" > "Medium").
encoded <- encoded %>%
  mutate(
    period_code = as.integer(factor(period,
                                    levels = c("2000-2007", "2008-2015", "2016-2022"))),
    energy_use_code = as.integer(factor(energy_use_level,
                                        levels = c("Low", "Medium", "High", "Very high"))))

cat("\n===== A3. Label encoding =====\n")
print(table(encoded$period, encoded$period_code))
print(table(encoded$energy_use_level, encoded$energy_use_code))

write.csv(encoded, file.path(data_dir, "energy_encoded.csv"), row.names = FALSE)
cat("Saved:", file.path(data_dir, "energy_encoded.csv"), "\n")

# =============================================================================
# PART B - PCA
# =============================================================================

# ---- B1. Choose the features ------------------------------------------------
# 13 numeric features describing size, wealth, energy use and the electricity
# mix. Deliberately LEFT OUT (to avoid counting the same information twice):
#  - renewables_share_elec, fossil_share_elec: exact sums of the source shares
#  - dominant_source dummies: derived from the shares already included
#  - energy_use_level / code: derived from energy_per_capita
# Skewed variables are log-transformed (as decided in the EDA) so that a few
# very large countries do not dominate the components.
pca_input <- energy %>%
  transmute(log_population     = log(population),
            log_gdp_pc         = log(gdp_per_capita),
            log_energy_pc      = log(energy_per_capita),
            log_elec_demand_pc = log(electricity_demand_per_capita),
            carbon_intensity_elec,
            coal_share_elec, gas_share_elec, oil_share_elec,
            nuclear_share_elec, hydro_share_elec,
            wind_share_elec, solar_share_elec,
            ghg_per_capita)

# ---- B2. Why standardise before PCA -----------------------------------------
# PCA looks for directions of MAXIMUM VARIANCE. Variables are on very
# different scales (carbon intensity 0-1,300 g vs log GDP ~6-12), so without
# scaling carbon intensity alone would dominate PC1. Z-score standardisation
# (mean 0, sd 1, as in Tutorial 3) gives every feature the same weight.
cat("\n===== B2. Variance BEFORE scaling (first 5) =====\n")
print(round(sort(sapply(pca_input, var), decreasing = TRUE)[1:5], 1))

pca_scaled <- scale(pca_input)          # z-score: (x - mean) / sd
cat("After scaling: every variance =", unique(round(apply(pca_scaled, 2, var), 6)), "\n")

# ---- B3. Run PCA -------------------------------------------------------------
# Data are already scaled; center/scale = TRUE would give the same result.
pca <- prcomp(pca_scaled, center = FALSE, scale. = FALSE)

# The sign of a component is arbitrary in PCA. PC1 is flipped so that
# "higher PC1 = richer / more energy use", which is easier to read.
pca$rotation[, 1] <- -pca$rotation[, 1]
pca$x[, 1]        <- -pca$x[, 1]

eig <- pca$sdev^2
var_tbl <- data.frame(PC = paste0("PC", seq_along(eig)),
                      eigenvalue = round(eig, 2),
                      variance_pct = round(100 * eig / sum(eig), 1),
                      cumulative_pct = round(100 * cumsum(eig) / sum(eig), 1))
cat("\n===== B3. Variance explained =====\n")
print(var_tbl)

# ---- B4. How many components to keep? ---------------------------------------
# Two common rules, both applied:
#  (1) Kaiser rule: keep components with eigenvalue > 1 (they explain more
#      than one original standardised variable).
#  (2) Cumulative variance of about 80%.
n_keep <- sum(eig > 1)
cat("\nKaiser rule keeps", n_keep, "components, explaining",
    var_tbl$cumulative_pct[n_keep], "% of the variance\n")
cat("-> 13 features reduced to", n_keep, "components (",
    round(100 * (1 - n_keep / 13)), "% fewer dimensions )\n")

p_scree <- var_tbl %>%
  mutate(PC = factor(PC, levels = PC), kept = seq_along(eig) <= n_keep) %>%
  ggplot(aes(x = PC)) +
  geom_col(aes(y = variance_pct, fill = kept)) +
  geom_line(aes(y = cumulative_pct, group = 1), colour = "#c1121f", linewidth = 1) +
  geom_point(aes(y = cumulative_pct), colour = "#c1121f", size = 2) +
  geom_hline(yintercept = 80, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 12, y = 83, label = "80%", colour = "grey40") +
  scale_fill_manual(values = c(`TRUE` = "#1f77b4", `FALSE` = "grey75"),
                    labels = c(`TRUE` = "Kept (eigenvalue > 1)", `FALSE` = "Dropped")) +
  labs(title = "Scree plot and cumulative variance",
       subtitle = paste0("Bars = variance per component; red line = cumulative. ",
                         n_keep, " components keep ", var_tbl$cumulative_pct[n_keep], "%."),
       x = NULL, y = "% of variance explained", fill = NULL) +
  theme_poster
ggsave("figures/03_scree_plot.png", p_scree, width = 9, height = 5, dpi = 150)

# ---- B5. Loadings: what does each component mean? ---------------------------
# A loading is the weight of an original variable in a component.
# Large |loading| (> ~0.3) = the variable is important for that component.
loadings <- as.data.frame(pca$rotation[, 1:n_keep])
cat("\n===== B5. Loadings of the kept components =====\n")
print(round(loadings, 2))

# Top variables of the first 3 components
for (k in 1:3) {
  l <- pca$rotation[, k]
  top <- l[order(-abs(l))][1:4]
  cat("\nPC", k, " main variables: ",
      paste0(names(top), " (", sprintf("%+.2f", top), ")", collapse = ", "), "\n", sep = "")
}
# Interpretation (written from the loadings above):
#  PC1 (~32%) "Development & energy use": log GDP, energy and electricity per
#      person and GHG per person all load strongly in the same direction.
#  PC2 (~17%) "Dirty vs clean electricity": carbon intensity and oil (+)
#      against hydro and nuclear (-).
#  PC3 (~12%) "Large coal systems vs new renewables": population and coal (+)
#      against wind, solar and oil (-).
# Because components are uncorrelated, PC1 (wealth) and PC2 (cleanliness)
# are SEPARATE dimensions - this confirms EDA Q3 (r = 0.14 between wealth and
# carbon intensity).

load_long <- loadings %>%
  mutate(variable = rownames(loadings)) %>%
  pivot_longer(-variable, names_to = "PC", values_to = "loading")

# Keep the variables in a logical order (wealth/energy, carbon, sources)
# instead of alphabetical, so the blocks of each component are easy to see
load_long$variable <- factor(load_long$variable, levels = rev(names(pca_input)))

p_load <- ggplot(load_long, aes(x = PC, y = variable, fill = loading)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", loading)), size = 3.2) +
  scale_fill_gradient2(low = "#c1121f", mid = "white", high = "#1f77b4",
                       limits = c(-0.7, 0.7)) +
  labs(title = "PCA loadings of the kept components",
       subtitle = "PC1 = development & energy use | PC2 = dirty vs clean | PC3 = coal vs wind/solar",
       x = NULL, y = NULL, fill = "Loading") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave("figures/03_loadings.png", p_load, width = 9, height = 6, dpi = 150)

# ---- B6. Countries in PCA space (link to the dataset) -----------------------
scores <- bind_cols(energy %>% select(country, year, dominant_source, period),
                    as.data.frame(pca$x[, 1:n_keep]))
write.csv(scores, file.path(data_dir, "pca_scores.csv"), row.names = FALSE)

s22 <- filter(scores, year == 2022)
label_pca <- c("Qatar", "Norway", "France", "Ethiopia", "Paraguay", "India",
               "China", "United States", "Ireland", "Nigeria", "Denmark", "Iraq")

p_bi <- ggplot(s22, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_point(aes(colour = dominant_source), size = 2.6, alpha = .85) +
  geom_text(data = filter(s22, country %in% label_pca),
            aes(label = country), size = 3.2, hjust = -0.12) +
  scale_colour_manual(values = source_cols) +
  labs(title = "Countries on the first two components, 2022",
       subtitle = "Right = richer, more energy. Up = dirtier electricity.\nThe dominant source (one-hot variable) separates countries vertically.",
       x = paste0("PC1: development & energy use (", var_tbl$variance_pct[1], "%)"),
       y = paste0("PC2: dirty vs clean (", var_tbl$variance_pct[2], "%)"),
       colour = "Dominant source") +
  theme_poster
ggsave("figures/03_pca_countries.png", p_bi, width = 9, height = 6.5, dpi = 150)

# ---- B7. Using the ENCODED variables to interpret the components ------------
# (a) One-hot dummies: correlation of each dummy with the components.
#     A strong correlation means that source is what the component captures.
cat("\n===== B7a. Correlation of one-hot dummies with PCs =====\n")
print(round(cor(encoded[, dummy_names], pca$x[, 1:3]), 2))

# (b) Label-encoded period: correlation with the components shows the
#     direction of the energy transition over time.
cat("\n===== B7b. Correlation of period_code with PCs =====\n")
print(round(cor(encoded$period_code, pca$x[, 1:3]), 2))

trend <- scores %>%
  group_by(year) %>%
  summarise(across(PC1:PC3, mean)) %>%
  pivot_longer(-year, names_to = "PC", values_to = "mean_score") %>%
  mutate(PC = recode(PC,
                     PC1 = "PC1 development & energy use",
                     PC2 = "PC2 dirty vs clean",
                     PC3 = "PC3 coal vs wind/solar"))

p_trend <- ggplot(trend, aes(year, mean_score, colour = PC)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("#1f77b4", "#c1121f", "#2ca02c")) +
  labs(title = "Average component score per year",
       subtitle = "PC1 rises (countries develop). PC2 and PC3 fall after ~2012:\nelectricity becomes cleaner and shifts towards wind/solar.",
       x = NULL, y = "Mean score (0 = average country-year)", colour = NULL) +
  theme_poster + guides(colour = guide_legend(nrow = 2))
ggsave("figures/03_pca_trend.png", p_trend, width = 8, height = 5, dpi = 150)

cat("\nAll PCA figures saved in figures/\n")

print(p_scree)   # scree plot e variância acumulada
print(p_load)    # loadings: o que cada componente significa
print(p_bi)      # países no PC1 x PC2 (2022)
print(p_trend)   # média dos componentes por ano



