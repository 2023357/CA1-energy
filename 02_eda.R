# =============================================================================
# CA1 - Data Exploration & Preparation
# Script 02: Exploratory Data Analysis (EDA) and unusual patterns
# Domain: ENERGY
#
# Input : data/energy_clean.csv   (created by 01_data_preparation.R)
# Output: figures/02_*.png        (7 plots for the poster)
#
# Following Lecture 4 (EDA), the analysis is driven by QUESTIONS:
#   Q1 (variation)   How is energy use per person distributed across countries?
#   Q2 (variation)   How did the electricity mix change between 2000 and 2022?
#   Q3 (covariation) Do richer countries have cleaner electricity?
#   Q4 (covariation) How does carbon intensity depend on the dominant source?
#   Q5 (unusual)     Did every country move towards renewables?
#   + correlation matrix of all numeric variables (overview of covariation,
#     which also prepares the ground for PCA in script 03)
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

dir.create("figures", showWarnings = FALSE)

# The clean CSV may be inside a "data" subfolder (GitHub layout) or next to
# this script (files downloaded separately). Both layouts work.
clean_file <- if (file.exists(file.path("data", "energy_clean.csv"))) {
  file.path("data", "energy_clean.csv")
} else {
  "energy_clean.csv"
}
if (!file.exists(clean_file)) {
  stop("energy_clean.csv not found. Run 01_data_preparation.R first, ",
       "or put energy_clean.csv in the working directory: ", getwd())
}
energy <- read.csv(clean_file, stringsAsFactors = FALSE)

# Fixed colour for each electricity source, used in every plot so the
# reader learns the colours once (fossil = warm/grey, clean = cool/green)
source_cols <- c(Coal = "#4a4a4a", Oil = "#8c564b", Gas = "#e07b39",
                 Nuclear = "#9467bd", Hydro = "#1f77b4", Wind = "#2ca02c")

# Common theme for all plots (larger text so it is readable on a poster)
theme_poster <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

cat("Clean data:", nrow(energy), "rows,", ncol(energy), "columns\n")


# =============================================================================
# Q1. How is energy use per person distributed across countries?  (VARIATION)
# =============================================================================
# Summary statistics first: mean vs median shows the skew
q1_stats <- energy %>%
  summarise(mean   = mean(energy_per_capita),
            median = median(energy_per_capita),
            sd     = sd(energy_per_capita),
            min    = min(energy_per_capita),
            max    = max(energy_per_capita))
cat("\n===== Q1. energy_per_capita (kWh/person) =====\n")
print(round(q1_stats))

# Histogram on the original scale vs log scale.
# The original scale is dominated by a long right tail; on the log scale
# the shape becomes readable (roughly symmetric).
p_q1 <- energy %>%
  filter(year == 2022) %>%                       # one year = one value per country
  ggplot(aes(x = energy_per_capita)) +
  geom_histogram(bins = 30, fill = "#1f77b4", colour = "white") +
  geom_vline(aes(xintercept = median(energy_per_capita)),
             linetype = "dashed", colour = "#c1121f") +
  scale_x_log10(labels = comma) +
  labs(title = "Q1. Energy use per person, 2022 (158 countries)",
       subtitle = "Log scale. Dashed red line = median. Qatar uses ~700x more than Burundi.",
       x = "Primary energy per person (kWh, log scale)", y = "Number of countries") +
  theme_poster
ggsave("figures/02_q1_energy_distribution.png", p_q1, width = 8, height = 5, dpi = 150)

# Extremes in 2022 (unusual values)
cat("\nLowest and highest energy use per person in 2022:\n")
e22 <- energy %>% filter(year == 2022) %>% arrange(energy_per_capita)
print(bind_rows(head(e22, 3), tail(e22, 3)) %>%
        select(country, energy_per_capita, gdp_per_capita))


# =============================================================================
# Q2. How did the electricity mix change between 2000 and 2022?  (VARIATION)
# =============================================================================
# Average share of each source per year (mean across countries).
# Only the 142 countries present in ALL 23 years are used, so the trend is
# not caused by countries entering or leaving the dataset.
balanced <- energy %>% group_by(country) %>% filter(n() == 23) %>% ungroup()
cat("\n===== Q2. Countries with all 23 years:", n_distinct(balanced$country), "=====\n")

mix_year <- balanced %>%
  group_by(year) %>%
  summarise(Coal = mean(coal_share_elec), Gas = mean(gas_share_elec),
            Oil = mean(oil_share_elec), Nuclear = mean(nuclear_share_elec),
            Hydro = mean(hydro_share_elec),
            `Wind + Solar` = mean(wind_share_elec + solar_share_elec)) %>%
  pivot_longer(-year, names_to = "source", values_to = "share")

print(mix_year %>% filter(year %in% c(2000, 2022)) %>%
        pivot_wider(names_from = year, values_from = share) %>%
        mutate(change = `2022` - `2000`) %>% arrange(desc(change)))

p_q2 <- ggplot(mix_year, aes(x = year, y = share, colour = source)) +
  geom_line(linewidth = 1.2) +
  geom_text(data = filter(mix_year, year == 2022),
            aes(label = source), hjust = -0.1, size = 3.8, show.legend = FALSE) +
  scale_colour_manual(values = c(source_cols, `Wind + Solar` = "#2ca02c")) +
  scale_x_continuous(limits = c(2000, 2026), breaks = seq(2000, 2020, 5)) +
  labs(title = "Q2. Average electricity mix, 2000-2022 (142 countries)",
       subtitle = "Wind + solar: ~0% to ~9%, mostly after 2010. Oil is being replaced by gas.",
       x = NULL, y = "Average share of electricity (%)") +
  theme_poster + theme(legend.position = "none")
ggsave("figures/02_q2_mix_over_time.png", p_q2, width = 8, height = 5, dpi = 150)


# =============================================================================
# Q3. Do richer countries have cleaner electricity?  (COVARIATION)
# =============================================================================
# Pearson correlation, using log(GDP per capita) because GDP is very skewed
cat("\n===== Q3. Correlations with log(GDP per capita) =====\n")
cat("carbon_intensity_elec :", round(cor(log(energy$gdp_per_capita),
                                          energy$carbon_intensity_elec), 2), "\n")
cat("renewables_share_elec :", round(cor(log(energy$gdp_per_capita),
                                          energy$renewables_share_elec), 2), "\n")

# Countries to label: the "unusual" ones that break the expected pattern
#  - rich but carbon-heavy (Qatar, Australia, Taiwan)
#  - rich and clean thanks to hydro/nuclear (Norway, France)
#  - poor but very clean thanks to hydro (Ethiopia, Paraguay)
#  - Turkmenistan/Uzbekistan: >1,100 g for gas-based systems, far above what
#    gas plants emit (~400-500 g). In the data Turkmenistan has the SAME value
#    (~1,306 g) every year -> probably an estimate, a data-quality warning.
label_q3 <- c("Qatar", "Australia", "Taiwan", "Norway", "France", "Ethiopia",
              "Paraguay", "India", "Ireland", "Turkmenistan", "Uzbekistan")

d22 <- energy %>% filter(year == 2022)
p_q3 <- ggplot(d22, aes(x = gdp_per_capita, y = carbon_intensity_elec)) +
  geom_point(aes(colour = dominant_source), size = 2.6, alpha = .8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = "black", linetype = "dashed", linewidth = .6) +
  geom_text(data = filter(d22, country %in% label_q3), aes(label = country),
            size = 3.2, hjust = -0.12) +
  scale_x_log10(labels = dollar) +
  scale_colour_manual(values = source_cols) +
  labs(title = "Q3. Wealth vs carbon intensity of electricity, 2022",
       subtitle = "Almost no relationship (r = 0.14). The dominant source matters more than wealth.",
       x = "GDP per person (log scale)", y = "gCO2 per kWh of electricity",
       colour = "Dominant source") +
  theme_poster
ggsave("figures/02_q3_gdp_vs_carbon.png", p_q3, width = 9, height = 6, dpi = 150)

# =============================================================================
# Q4. How does carbon intensity depend on the dominant source?  (COVARIATION)
# =============================================================================
# Categorical vs continuous -> boxplot, ordered by the median (Lecture 4)
cat("\n===== Q4. Carbon intensity by dominant source =====\n")
print(energy %>% group_by(dominant_source) %>%
        summarise(n = n(), median_gCO2 = median(carbon_intensity_elec),
                  IQR = IQR(carbon_intensity_elec)) %>% arrange(median_gCO2))

p_q4 <- ggplot(energy, aes(x = reorder(dominant_source, carbon_intensity_elec, median),
                           y = carbon_intensity_elec, fill = dominant_source)) +
  geom_boxplot(outlier.alpha = .3, show.legend = FALSE) +
  scale_fill_manual(values = source_cols) +
  labs(title = "Q4. Carbon intensity by dominant source (all years)",
       subtitle = "Ordered by median. Hydro-led systems emit ~4.5x less than coal-led ones (148 vs 666 g).",
       x = "Dominant source of electricity", y = "gCO2 per kWh") +
  theme_poster
ggsave("figures/02_q4_carbon_by_source.png", p_q4, width = 8, height = 5, dpi = 150)

# Two categorical variables: dominant source x period (geom_count, Lecture 4)
cat("\nDominant source by period (country-years):\n")
print(table(energy$dominant_source, energy$period))

p_q4b <- ggplot(energy, aes(x = period, y = dominant_source)) +
  geom_count(aes(colour = dominant_source), show.legend = FALSE) +
  scale_size_area(max_size = 14) +
  scale_colour_manual(values = source_cols) +
  labs(title = "Dominant source by period",
       subtitle = "Oil and hydro dominate less; gas and wind more.",
       x = NULL, y = NULL) +
  theme_poster
ggsave("figures/02_q4b_source_by_period.png", p_q4b, width = 7, height = 5, dpi = 150)


# =============================================================================
# Q5. Did every country move towards renewables?  (UNUSUAL PATTERN)
# =============================================================================
change <- energy %>%
  filter(year %in% c(2000, 2022)) %>%
  select(country, year, renewables_share_elec) %>%
  pivot_wider(names_from = year, values_from = renewables_share_elec,
              names_prefix = "y") %>%
  filter(!is.na(y2000), !is.na(y2022)) %>%
  mutate(change = y2022 - y2000)

cat("\n===== Q5. Change in renewables share 2000 -> 2022 =====\n")
cat("Countries that increased:", sum(change$change > 0),
    " decreased:", sum(change$change < 0), "\n")

top_bottom <- bind_rows(slice_max(change, change, n = 8),
                        slice_min(change, change, n = 8)) %>%
  mutate(direction = ifelse(change > 0, "Increase", "Decrease"))
print(top_bottom %>% select(country, y2000, y2022, change))

p_q5 <- ggplot(top_bottom, aes(x = reorder(country, change), y = change, fill = direction)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_manual(values = c(Increase = "#2ca02c", Decrease = "#c1121f")) +
  labs(title = "Q5. Largest changes in renewable share, 2000-2022",
       subtitle = "Unusual: 41 countries became LESS renewable, mostly hydro-based African\nsystems where fossil generation grew faster than hydro as demand rose.",
       x = NULL, y = "Change in renewables share (percentage points)", fill = NULL) +
  theme_poster
ggsave("figures/02_q5_renewables_change.png", p_q5, width = 8, height = 6, dpi = 150)


# =============================================================================
# Overview: correlation matrix of numeric variables  (COVARIATION)
# =============================================================================
num_vars <- energy %>%
  select(population, gdp_per_capita, energy_per_capita,
         electricity_demand_per_capita, carbon_intensity_elec,
         coal_share_elec, gas_share_elec, oil_share_elec, nuclear_share_elec,
         hydro_share_elec, wind_share_elec, solar_share_elec, ghg_per_capita) %>%
  # skewed size/wealth variables are logged so Pearson r is not driven by outliers
  mutate(across(c(population, gdp_per_capita, energy_per_capita,
                  electricity_demand_per_capita), log))

cor_mat <- cor(num_vars)
cor_long <- as.data.frame(as.table(cor_mat)) %>% rename(r = Freq)

cat("\n===== Strongest correlations (|r| > 0.6) =====\n")
print(cor_long %>% filter(as.integer(Var1) < as.integer(Var2), abs(r) > 0.6) %>%
        mutate(r = round(r, 2)) %>% arrange(desc(abs(r))))

p_cor <- ggplot(cor_long, aes(Var1, Var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.7) +
  scale_fill_gradient2(low = "#c1121f", mid = "white", high = "#1f77b4",
                       limits = c(-1, 1)) +
  labs(title = "Correlation matrix (size/wealth variables logged)",
       x = NULL, y = NULL, fill = "r") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))
ggsave("figures/02_correlation_matrix.png", p_cor, width = 9, height = 7.5, dpi = 150)

cat("\nAll EDA figures saved in figures/\n")

print(p_q1)    # Q1 - distribuição do consumo de energia
print(p_q2)    # Q2 - matriz elétrica 2000-2022
print(p_q3)    # Q3 - PIB x carbono
print(p_q4)    # Q4 - carbono por fonte (boxplot)
print(p_q4b)   # Q4 - fonte dominante por período
print(p_q5)    # Q5 - países que mais mudaram
print(p_cor)   # matriz de correlação


