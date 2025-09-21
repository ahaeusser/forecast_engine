
# Hyperparameter Sweep ########################################################

# Pre-processing --------------------------------------------------------------

source("parameter/pars_100_config_file.R")

# Directory and file names, forecast horizon and period
files <- list(
  "output/20250912_pars_monthly_aic.rds",
  "output/20250918_pars_monthly_aicc.rds",
  "output/20250920_pars_monthly_bic.rds",
  "output/20250904_pars_quarterly_aic.rds",
  "output/20250904_pars_quarterly_aicc.rds",
  "output/20250905_pars_quarterly_bic.rds",
  "output/20250905_pars_quarterly_hqc.rds"
)

# Read rds-files and combine data frames row-wise
pars_frame <- bind_rows(lapply(files, readRDS))

# Table main text (top n models, one table per frequency) ---------------------

# set_freq <- "monthly"
set_freq <- "quarterly"
n_rows <- 30

# Average sMAPE and MASE and combine with hyperparameters
pars_model <- pars_frame %>%
  filter(freq == set_freq) %>%
  group_by(model) %>%
  summarise(
    mase_mean    = round(mean(mase, na.rm = TRUE), 3),
    mase_median  = round(median(mase, na.rm = TRUE), 3),
    smape_mean   = round(mean(smape, na.rm = TRUE), 3),
    smape_median = round(median(smape, na.rm = TRUE), 3)
  ) %>%
  ungroup()

pars_model <- left_join(
  x = pars,
  y = pars_model,
  by = "model"
)

pars_model <- pars_model %>%
  arrange(mase_mean) %>%
  mutate(
    inf_crit = recode(
      inf_crit,
      "aic" = "AIC",
      "bic" = "BIC",
      "aicc" = "AICc",
      "hqc" = "HQC"
    )
  )

# Create table as LaTeX code
pars_model %>%
  slice_head(n = n_rows) %>%
  mutate(rank = row_number()) %>%
  select(rank, model, inf_crit, alpha, rho, tau, mase_mean, mase_median, smape_mean, smape_median) %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()


# Prepare data per frequency and hyperparameter -------------------------------

# Leakage rate alpha
pars_alpha <- pars_frame %>%
  group_by(freq, alpha) %>%
  summarise(
    "mase_min"     = round(min(mase, na.rm = TRUE), 3),
    "mase_q1"      = round(quantile(x = mase, probs = 0.25), 3),
    "mase_mean"    = round(mean(mase, na.rm = TRUE), 3),
    "mase_median"  = round(median(mase, na.rm = TRUE), 3),
    "mase_q3"      = round(quantile(x = mase, probs = 0.75), 3),
    "mase_max"     = round(max(mase, na.rm = TRUE), 3),
    "mase_std"     = round(sd(mase, na.rm = TRUE), 3),
    "smape_min"    = round(min(smape, na.rm = TRUE), 3),
    "smape_q1"     = round(quantile(x = smape, probs = 0.25), 3),
    "smape_mean"   = round(mean(smape, na.rm = TRUE), 3),
    "smape_median" = round(median(smape, na.rm = TRUE), 3),
    "smape_q3"     = round(quantile(x = smape, probs = 0.75), 3),
    "smape_max"    = round(max(smape, na.rm = TRUE), 3),
    "smape_std"    = round(sd(smape, na.rm = TRUE), 3),
    .groups = "drop") %>%
  rename(value = alpha) %>%
  mutate(value = as_factor(value)) %>%
  mutate(par = "alpha", .before = value)


# Spectral radius rho
pars_rho <- pars_frame %>%
  group_by(freq, rho) %>%
  summarise(
    "mase_min"     = round(min(mase, na.rm = TRUE), 3),
    "mase_q1"      = round(quantile(x = mase, probs = 0.25), 3),
    "mase_mean"    = round(mean(mase, na.rm = TRUE), 3),
    "mase_median"  = round(median(mase, na.rm = TRUE), 3),
    "mase_q3"      = round(quantile(x = mase, probs = 0.75), 3),
    "mase_max"     = round(max(mase, na.rm = TRUE), 3),
    "mase_std"     = round(sd(mase, na.rm = TRUE), 3),
    "smape_min"    = round(min(smape, na.rm = TRUE), 3),
    "smape_q1"     = round(quantile(x = smape, probs = 0.25), 3),
    "smape_mean"   = round(mean(smape, na.rm = TRUE), 3),
    "smape_median" = round(median(smape, na.rm = TRUE), 3),
    "smape_q3"     = round(quantile(x = smape, probs = 0.75), 3),
    "smape_max"    = round(max(smape, na.rm = TRUE), 3),
    "smape_std"    = round(sd(smape, na.rm = TRUE), 3),
    .groups = "drop") %>%
  rename(value = rho) %>%
  mutate(value = as_factor(value)) %>%
  mutate(par = "rho", .before = value)


# Reservoir scaling tau
pars_tau <- pars_frame %>%
  group_by(freq, tau) %>%
  summarise(
    "mase_min"     = round(min(mase, na.rm = TRUE), 3),
    "mase_q1"      = round(quantile(x = mase, probs = 0.25), 3),
    "mase_mean"    = round(mean(mase, na.rm = TRUE), 3),
    "mase_median"  = round(median(mase, na.rm = TRUE), 3),
    "mase_q3"      = round(quantile(x = mase, probs = 0.75), 3),
    "mase_max"     = round(max(mase, na.rm = TRUE), 3),
    "mase_std"     = round(sd(mase, na.rm = TRUE), 3),
    "smape_min"    = round(min(smape, na.rm = TRUE), 3),
    "smape_q1"     = round(quantile(x = smape, probs = 0.25), 3),
    "smape_mean"   = round(mean(smape, na.rm = TRUE), 3),
    "smape_median" = round(median(smape, na.rm = TRUE), 3),
    "smape_q3"     = round(quantile(x = smape, probs = 0.75), 3),
    "smape_max"    = round(max(smape, na.rm = TRUE), 3),
    "smape_std"    = round(sd(smape, na.rm = TRUE), 3),
    .groups = "drop") %>%
  rename(value = tau) %>%
  mutate(value = as_factor(value)) %>%
  mutate(par = "tau", .before = value)


# Information criterion
pars_inf_crit <- pars_frame %>%
  group_by(freq, inf_crit) %>%
  summarise(
    "mase_min"     = round(min(mase, na.rm = TRUE), 3),
    "mase_q1"      = round(quantile(x = mase, probs = 0.25), 3),
    "mase_mean"    = round(mean(mase, na.rm = TRUE), 3),
    "mase_median"  = round(median(mase, na.rm = TRUE), 3),
    "mase_q3"      = round(quantile(x = mase, probs = 0.75), 3),
    "mase_max"     = round(max(mase, na.rm = TRUE), 3),
    "mase_std"     = round(sd(mase, na.rm = TRUE), 3),
    "smape_min"    = round(min(smape, na.rm = TRUE), 3),
    "smape_q1"     = round(quantile(x = smape, probs = 0.25), 3),
    "smape_mean"   = round(mean(smape, na.rm = TRUE), 3),
    "smape_median" = round(median(smape, na.rm = TRUE), 3),
    "smape_q3"     = round(quantile(x = smape, probs = 0.75), 3),
    "smape_max"    = round(max(smape, na.rm = TRUE), 3),
    "smape_std"    = round(sd(smape, na.rm = TRUE), 3),
    .groups = "drop") %>%
  mutate(
    inf_crit = recode(
      inf_crit,
      "aic" = "AIC",
      "bic" = "BIC",
      "aicc" = "AICc",
      "hqc" = "HQC")) %>%
  rename(value = inf_crit) %>%
  mutate(value = as_factor(value)) %>%
  mutate(par = "inf_crit", .before = value)


pars_dist <- bind_rows(
  pars_alpha,
  pars_rho,
  pars_tau,
  pars_inf_crit)


# Tables appendix (one table per frequency) -----------------------------------

# set_freq <- "monthly"
set_freq <- "quarterly"

# Create table as LaTeX code
pars_dist %>%
  filter(freq == set_freq) %>%
  select(-freq) %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()


# Figure (one figure for both frequencies) ------------------------------------

# set_metric <- "mase_mean"
set_metric <- "mase_median"
# set_metric <- "smape_mean"
# set_metric <- "smape_median"

# Reorder facets manually
pars_dist$par <- factor(
  pars_dist$par,
  levels = c(
    "alpha", 
    "rho", 
    "tau", 
    "inf_crit"
  )
)

pars_dist <- pars_dist %>%
  mutate(
    par = recode(
      par,
      "alpha" = "Leakage Rate",
      "rho" = "Spectral Radius",
      "tau" = "Reservoir Scaling",
      "inf_crit" = "Information Criterion")) %>%
  mutate(
    freq = recode(
      freq,
      "monthly" = "Monthly",
      "quarterly" = "Quarterly"))

# find row(s) with minimum mase_mean per facet
min_points <- pars_dist %>%
  group_by(freq, par) %>%
  slice_min(!!sym(set_metric), n = 1, with_ties = FALSE) %>%
  ungroup()


p <- ggplot(
  data = pars_dist,
  aes(
    x = factor(value),
    y = !!sym(set_metric),
    group = 1)
)

p <- p + geom_point(color = "grey35", size = 4)
p <- p + geom_line(color = "grey35", size = 1)

p <- p + geom_point(
  data = min_points,
  aes(
    x = factor(value), 
    y = !!sym(set_metric)),
  color = "#F8766D", 
  size = 5)

p <- p + facet_wrap(par ~ freq, scales = "free", ncol = 2)
p <- p + coord_flip()
p <- p + labs(x = "Hyperparameter", y = set_metric)
p <- p + theme_tscv()
p
