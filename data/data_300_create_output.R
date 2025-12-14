
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



# Summary datasets Parameter and Forecast =====================================

table_dataset <- bind_rows(
  meta_mth_parameter %>% select(-c(start, end)),
  meta_mth_forecast %>% select(-c(start, end)),
  meta_qtr_parameter %>% select(-c(start, end)),
  meta_qtr_forecast %>% select(-c(start, end))
)

table_dataset <- table_dataset %>%
  group_by(category, dataset, freq) %>%
  summarise(absolute = n()) %>%
  arrange(dataset, freq, category, desc(absolute)) %>%
  rename(Category = category) %>%
  rename("Absolute (n)" = absolute)


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



# Figure 1 --------------------------------------------------------------------

# Exclude series with too many observations
meta_frame2 <- meta_frame %>%
  filter(case_when(freq == "Monthly" ~ value < 600,
                   freq == "Quarterly" ~ value < 300))


p <- ggplot()

p <- p + geom_density(
  data = meta_frame2,
  aes(x = value, color = dataset, fill = dataset),
  alpha = 0.05,
  size = 0.8
)

# p <- p + scale_x_log10()
# p <- p + scale_x_continuous(trans = "exp")

p <- p + scale_color_manual(values=c("grey35", "#00BFC4", "#F8766D"))
p <- p + scale_fill_manual(values=c("grey35", "#00BFC4", "#F8766D"))

p <- p + facet_wrap(
  freq ~ metric,
  scales = "free",
  nrow = 2
)

p <- p + labs(x = "Value")
p <- p + labs(y = "Density")
p <- p + theme_tscv()
p


figure_name <- "output/figure_01_data_summary.pdf"
fig_width <- 17
fig_hight <- 12

ggsave(
  filename = figure_name,
  width = fig_width,
  height = fig_hight,
  units = "cm"
)



# Figure 2 --------------------------------------------------------------------

set_series <- c(
  "M21655",
  "M28597",
  "M39525",
  "M2717"
)

# Prepare data

data_sample <- main_mth_total %>%
  filter(series %in% set_series) %>%
  mutate(series = paste0(series, " (", category, ")")) %>%
  mutate(index = as.Date(index))

# Plot data as line chart
p <- ggplot()

p <- p + geom_line(
  data = data_sample,
  aes(
    x = index,
    y = value),
  linewidth = 0.5,
  color = "grey35"
)

p <- p + facet_wrap(
  vars(series),
  ncol = 2,
  scales = "free")

p <- p + labs(x = "Time")
p <- p + labs(y = "Value")
p <- p + scale_x_date(labels = scales::label_date_short())
p <- p + theme_tscv()
p <- p + theme(legend.position = "none")

p

figure_name <- "output/figure_02_data_sample.pdf"
fig_width <- 17
fig_hight <- 10

ggsave(
  filename = figure_name,
  width = fig_width,
  height = fig_hight,
  units = "cm"
)

