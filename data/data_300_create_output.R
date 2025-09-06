
library(tidyverse)
library(tscv)
library(feasts)
library(tsibble)

# Pre-processing --------------------------------------------------------------

# Directory and file names
files_mth <- list(
  "data/main_mth_total.rds",
  "data/main_mth_parameter.rds",
  "data/main_mth_forecast.rds"
)

# Read rds-files and combine data frames row-wise
main_frame_mth <- bind_rows(lapply(files_mth, readRDS))

# Directory and file names
files_qtr <- list(
  "data/main_qtr_total.rds",
  "data/main_qtr_parameter.rds",
  "data/main_qtr_forecast.rds"
)

# Read rds-files and combine data frames row-wise
main_frame_qtr <- bind_rows(lapply(files_qtr, readRDS))

# Figure 1 --------------------------------------------------------------------
# Exploratory data analysis (trend, seasonality, etc.)

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


plot <- dml(ggobj = p)
pptx <- read_pptx()
pptx <- add_slide(x = pptx)
pptx <- ph_with(
  x = pptx,
  value = plot,
  location = ph_location(
    "body",
    left = 1,
    top = 1,
    width = 8,
    height = 5
  )
)

print(
  x = pptx,
  target = "slides/figure_01_data_monthly.pptx"
)