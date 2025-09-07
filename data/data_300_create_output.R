
library(tidyverse)
library(tscv)
library(feasts)
library(tsibble)
library(gt)

# Load data ===================================================================

# Monthly data ----------------------------------------------------------------

main_mth_total <- readRDS(file = "data/main_mth_total.rds")
meta_mth_total <- readRDS(file = "data/meta_mth_total.rds")
meta_mth_parameter <- readRDS(file = "data/meta_mth_parameter.rds")
meta_mth_forecast <- readRDS(file = "data/meta_mth_forecast.rds")

# Quarterly data --------------------------------------------------------------

main_qtr_total <- readRDS(file = "data/main_qtr_total.rds")
meta_qtr_total <- readRDS(file = "data/meta_qtr_total.rds")
meta_qtr_parameter <- readRDS(file = "data/meta_qtr_parameter.rds")
meta_qtr_forecast <- readRDS(file = "data/meta_qtr_forecast.rds")


# Estimate STL features (strength of trend and seasonality) ===================

# Monthly data ----------------------------------------------------------------

feature_frame_mth <- main_mth_total %>%
  as_tsibble(key = "series", index = "index") %>%
  features(value, feat_stl) %>%
  select(series, trend_strength, seasonal_strength_year)

meta_frame_mth <- left_join(
  x = meta_mth_total,
  y = feature_frame_mth,
  by = "series"
)

series_mth_pars <- unique(meta_mth_parameter[["series"]])
series_mth_fcst <- unique(meta_mth_forecast[["series"]])

meta_frame_mth_pars <- meta_frame_mth %>%
  filter(series %in% series_mth_pars) %>%
  mutate(dataset = "Parameter")

meta_frame_mth_fcst <- meta_frame_mth %>%
  filter(series %in% series_mth_fcst) %>%
  mutate(dataset = "Forecast")

meta_frame_mth <- bind_rows(
  meta_frame_mth,
  meta_frame_mth_pars,
  meta_frame_mth_fcst
)

# Quarterly data --------------------------------------------------------------

feature_frame_qtr <- main_qtr_total %>%
  as_tsibble(key = "series", index = "index") %>%
  features(value, feat_stl) %>%
  select(series, trend_strength, seasonal_strength_year)

meta_frame_qtr <- left_join(
  x = meta_qtr_total,
  y = feature_frame_qtr,
  by = "series"
)

series_qtr_pars <- unique(meta_qtr_parameter[["series"]])
series_qtr_fcst <- unique(meta_qtr_forecast[["series"]])

meta_frame_qtr_pars <- meta_frame_qtr %>%
  filter(series %in% series_qtr_pars) %>%
  mutate(dataset = "Parameter")

meta_frame_qtr_fcst <- meta_frame_qtr %>%
  filter(series %in% series_qtr_fcst) %>%
  mutate(dataset = "Forecast")

meta_frame_qtr <- bind_rows(
  meta_frame_qtr,
  meta_frame_qtr_pars,
  meta_frame_qtr_fcst
)

# Combine datasets
meta_frame <- bind_rows(
  meta_frame_mth %>% select(-c(start, end)),
  meta_frame_qtr %>% select(-c(start, end))
)

meta_frame <- meta_frame %>%
  select(-n_ahead) %>%
  rename(
    "Length" = n_obs,
    "Trend" = trend_strength,
    "Season" = seasonal_strength_year) %>%
  pivot_longer(
    cols = -c(series, category, freq, dataset),
    names_to = "metric",
    values_to = "value") %>%
  arrange(series, freq, metric)


# Reorder factor levels of frequency
meta_frame$freq <- factor(
  meta_frame$freq,
  levels = c(
    "Monthly", 
    "Quarterly"
  )
)

# Reorder factor levels of dataset
meta_frame$dataset <- factor(
  meta_frame$dataset,
  levels = c(
    "Total", 
    "Parameter",
    "Forecast"
  )
)

# Estimate summary statistics
table_meta <- meta_frame %>%
  group_by(freq, metric, dataset) %>%
  summarise(
    "Min"    = round(min(value, na.rm = TRUE), 3),
    "Q25"    = round(quantile(x = value, probs = 0.25), 3),
    "Mean"   = round(mean(value, na.rm = TRUE), 3),
    "Median" = round(median(value, na.rm = TRUE), 3),
    "Q75"    = round(quantile(x = value, probs = 0.75), 3),
    "Max"    = round(max(value, na.rm = TRUE), 3),
    "Std"    = round(sd(value, na.rm = TRUE), 3),
    .groups = "drop")

table_meta %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()





meta_frame <- meta_frame %>%
  filter(value <= 500)

p <- ggplot()

p <- p + geom_density(
  data = meta_frame,
  aes(x = value, color = dataset, fill = dataset),
  alpha = 0.05,
  size = 0.8
)

# p <- p + scale_x_log10()

p <- p + facet_wrap(
  freq ~ metric,
  scales = "free",
  nrow = 2
)

p <- p + labs(x = "Value")
p <- p + labs(y = "Density")
p <- p + theme_tscv()

p


figure_name <- paste0("figure_01_data_monthly",".pdf")

ggsave(
  filename = figure_name,
  width = 17, # 25
  height = 15, # 10
  units = "cm"
)

