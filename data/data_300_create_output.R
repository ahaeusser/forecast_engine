
library(tidyverse)
library(tscv)
library(feasts)
library(tsibble)

# Load data ===================================================================

### TO DO #####################################################################
# main_mth_total
# meta_mth_total
# meta_mth_forecast
# meta_mth_parameter

# Main files ------------------------------------------------------------------

# Directory and file names
files_main_mth <- list(
  "data/main_mth_total.rds",
  "data/main_mth_parameter.rds",
  "data/main_mth_forecast.rds"
)

# Read rds-files and combine data frames row-wise
main_frame_mth <- bind_rows(lapply(files_main_mth, readRDS))

# Directory and file names
files_main_qtr <- list(
  "data/main_qtr_total.rds",
  "data/main_qtr_parameter.rds",
  "data/main_qtr_forecast.rds"
)

# Read rds-files and combine data frames row-wise
main_frame_qtr <- bind_rows(lapply(files_main_qtr, readRDS))

# Meta files ------------------------------------------------------------------

# Directory and file names
files_meta_mth <- list(
  "data/meta_mth_total.rds",
  "data/meta_mth_parameter.rds",
  "data/meta_mth_forecast.rds"
)

# Read rds-files and combine data frames row-wise
meta_frame_mth <- bind_rows(lapply(files_meta_mth, readRDS))

# Directory and file names
files_meta_qtr <- list(
  "data/meta_qtr_total.rds",
  "data/meta_qtr_parameter.rds",
  "data/meta_qtr_forecast.rds"
)

# Read rds-files and combine data frames row-wise
meta_frame_qtr <- bind_rows(lapply(files_meta_qtr, readRDS))

# Exploratory data analysis (trend, seasonality, etc.) ========================

# Prepare data
feature_frame_mth <- main_frame_mth %>%
  as_tsibble(
    key = c("series", "dataset"),
    index = "index") %>%
  features(value, feat_stl) %>%
  select(series, trend_strength, seasonal_strength_year)

feature_frame_qtr <- main_frame_qtr %>%
  as_tsibble(
    key = c("series", "dataset"),
    index = "index") %>%
  features(value, feat_stl) %>%
  select(series, trend_strength, seasonal_strength_year)

feature_frame <- bind_rows(
  feature_frame_mth,
  feature_frame_qtr
)




meta_frame <- bind_rows(
  meta_frame_mth %>% select(-c(start, end)),
  meta_frame_qtr %>% select(-c(start, end))
)

meta_frame <- left_join(
  x = meta_frame,
  y = feature_frame,
  by = "series"
)


meta_frame <- meta_frame %>%
  select(-c(category, n_ahead)) %>%
  pivot_longer(
    cols = -c(series, freq, dataset),
    names_to = "metric",
    values_to = "value") %>%
  arrange(series, freq, metric)

table_meta <- meta_frame %>%
  group_by(freq, metric, dataset) %>%
  summarise(
    "Min"    = round(min(value, na.rm = TRUE), 3),
    "Q25"    = round(quantile(x = value, probs = 0.25), 3),
    "Mean"   = round(mean(value, na.rm = TRUE), 3),
    "Median" = round(median(value, na.rm = TRUE), 3),
    "Q75"    = round(quantile(x = value, probs = 0.75), 3),
    "Max"    = round(max(value, na.rm = TRUE), 3),
    "Std"    = round(sd(value, na.rm = TRUE), 3)) %>%
  ungroup() %>%
  arrange(freq, metric, dataset)

table_meta %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()




meta_frame <- meta_frame %>%
  mutate(across(freq, factor, levels=c("Monthly","Quarterly"))) %>%
  mutate(across(dataset, factor, levels=c("M4", "Sample"))) %>%
  mutate(metric = recode(
    metric, 
    n_obs = "Number of obs.",
    seasonal_strength_year = "Seasonal strength",
    trend_strength = "Trend strength")) 




###

meta_frame_season <- meta_frame %>%
  filter(freq == "Monthly") %>%
  filter(metric == "Seasonal strength") %>%
  arrange(desc(value))

###



set_freq <- "Monthly"

meta_frame2 <- meta_frame %>%
  filter(freq == set_freq)

p <- ggplot()

p <- p + geom_histogram(
  data = meta_frame2,
  aes(x = value),
  position = "identity",
  alpha = 0.5
)

p <- p + facet_wrap(
  vars(dataset, metric),
  scales = "free",
  nrow = 2
)

p <- p + labs(x = "Value")
p <- p + labs(y = "Absolute frequency")
p <- p + theme_tscv()

p


figure_name <- paste0("figures/figure_01_data_monthly",".pdf")

ggsave(
  filename = figure_name,
  width = 17, # 25
  height = 10, # 10
  units = "cm"
)

