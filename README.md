
<!-- README.md is generated from README.Rmd. Please edit README.Rmd. -->

# Echo State Networks for Time Series Forecasting

This repository contains the replication code for *Echo State Networks
for Time Series Forecasting: Hyperparameter Sweep and Benchmarking*. The
paper examines whether a simple first-order autoregressive Echo State
Network (ESN) can provide competitive forecasts for monthly and
quarterly time series from the M4 Forecasting Competition.

The empirical analysis consists of two main stages:

1.  A **Parameter sample** is used to evaluate ESN configurations across
    leakage rate, spectral radius, reservoir size, and the information
    criterion used for selecting the regularization parameter.
2.  A separate **Forecast sample** is used for the final comparison with
    naive, drift, seasonal naive, mean, ARIMA, ETS, Theta, TBATS, MLP,
    RNN, and ES-RNN forecasts.

Each sample contains 2,400 monthly and 1,200 quarterly series with no
more than 20 years of history. Forecast accuracy is evaluated primarily
using MASE and sMAPE over the M4 forecast horizons of 18 months and 8
quarters.

The repository follows the sequence of the paper: prepare the data,
evaluate the ESN hyperparameters, run the forecast benchmark, and create
the final tables and figures.

## Data

The analysis uses the official [M4 competition
repository](https://github.com/Mcompetitions/M4-methods) as its primary
data source.

The relevant parts of the M4 repository are:

- `Dataset/`, which contains the training data, test data, and
  series-level information;
- `Point Forecasts/`, which contains the forecasts from the valid M4
  submissions, competition benchmarks, and standards of comparison.

The prepared files included in `data/` are the direct starting point for
the replication. They contain the monthly and quarterly observations,
metadata, sample assignments, and imported M4 forecasts required by the
analysis.

Files ending in `_parameter` belong to the Parameter sample, while files
ending in `_forecast` belong to the Forecast sample. Files ending in
`_total` describe the broader monthly or quarterly M4 collection from
which the samples were constructed.

## Requirements

Use [R 4.1 or newer](https://cran.r-project.org/) and run the scripts
from the project root. The easiest approach is to open
`forecast_engine.Rproj` in
[RStudio](https://posit.co/download/rstudio-desktop/).

The analysis is built around the following package groups:

- **Data handling and general workflows:**  
  [`tidyverse`](https://cran.r-project.org/package=tidyverse),
  [`lubridate`](https://cran.r-project.org/package=lubridate),
  [`fs`](https://cran.r-project.org/package=fs), and
  [`glue`](https://cran.r-project.org/package=glue).

- **Tidy time series infrastructure:**  
  [`tsibble`](https://cran.r-project.org/package=tsibble),
  [`fable`](https://cran.r-project.org/package=fable),
  [`fabletools`](https://cran.r-project.org/package=fabletools),
  [`feasts`](https://cran.r-project.org/package=feasts), and
  [`distributional`](https://cran.r-project.org/package=distributional).

- **Forecasting models and evaluation:**  
  [`echos`](https://cran.r-project.org/package=echos) for Echo State
  Networks, [`forecast`](https://cran.r-project.org/package=forecast)
  for additional statistical forecasting methods, and
  [`tscv`](https://cran.r-project.org/package=tscv) for additional
  benchmark models and forecast evaluation tools.

- **Parallel execution and progress reporting:**  
  [`future`](https://cran.r-project.org/package=future),
  [`furrr`](https://cran.r-project.org/package=furrr), and
  [`progressr`](https://cran.r-project.org/package=progressr).

- **Tables, figures, and reporting:**  
  [`gt`](https://cran.r-project.org/package=gt),
  [`patchwork`](https://cran.r-project.org/package=patchwork), and
  [`rmarkdown`](https://cran.r-project.org/package=rmarkdown).

Install all packages used by the replication scripts with:

``` r
install.packages(c(
  "crayon",
  "devtools",
  "distributional",
  "echos",
  "fable",
  "fabletools",
  "feasts",
  "forecast",
  "fs",
  "furrr",
  "future",
  "glue",
  "gt",
  "lubridate",
  "patchwork",
  "progressr",
  "pryr",
  "rmarkdown",
  "rstudioapi",
  "tictoc",
  "tidyverse",
  "tscv",
  "tsibble"
))
```

All file paths used by the scripts are relative to the project root.

## Project structure

| Folder | Contents |
|----|----|
| `data/` | Prepared M4 observations, metadata, sample assignments, and imported competition forecasts. The numbered `data_*.R` scripts document the preparation of the analysis samples and descriptive outputs. |
| `parameter/` | Configuration, training, evaluation, and reporting scripts for the ESN hyperparameter sweep. Dated `.rds` files are intermediate run results; the curated results used in the paper are stored in `output/`. |
| `random/` | Scripts for evaluating the sensitivity of the ESN forecasts to different random reservoir initializations. |
| `forecast/` | Configuration, model, evaluation, and reporting scripts for the final forecast benchmark. Dated run directories are intermediate workspaces created during model estimation. |
| `output/` | Curated data objects, result tables, and figures used in the paper. |
| `R/` | Helper functions for logging, file handling, imported forecasts, and data processing. |

## Workflow

### 1. Prepare the data

The prepared files in `data/` can be used directly to reproduce the
hyperparameter sweep and forecast benchmark.

To reconstruct these inputs from the official M4 files, use the numbered
data scripts as a guide:

1.  `data/data_100_prep_data_full.R` imports and reshapes the monthly or
    quarterly training data, test data, and metadata from the official
    M4 repository.
2.  `data/data_200_prep_data_sample.R` applies the history-length
    restriction and creates the Parameter and Forecast samples. Run the
    script separately for each frequency and sample.
3.  `data/data_300_create_output.R` calculates the descriptive summaries
    and writes the corresponding figures to `output/`.

The M4 participant and benchmark forecasts used in the final comparison
are imported from the `Point Forecasts/` directory of the official M4
repository and converted into the format required by the replication
scripts.

Review the frequency, sample, random seed, and save settings near the
beginning of each data script before running it.

### 2. Run the hyperparameter sweep

The scripts in `parameter/` form a numbered workflow:

1.  `pars_100_config_file.R` defines the accuracy measures and the
    1,320-configuration ESN grid.
2.  `pars_200_train_models.R` evaluates one frequency and one
    information criterion per execution.
3.  Run all eight combinations formed by crossing monthly and quarterly
    data with AIC, AICc, BIC, and HQC.
4.  Retain the eight dated `.rds` result files and place the curated
    copies in `output/` using the filenames referenced by
    `pars_300_create_output.R`.
5.  Run `pars_300_create_output.R` to combine the results and create the
    hyperparameter tables and summary figure.
6.  Run `pars_400_pars_figure.R` to create the illustrative figure
    showing how the leakage rate and spectral radius affect the
    forecasts.

Set `test_run <- FALSE` in `pars_200_train_models.R` for the complete
analysis.

The full sweep evaluates 1,320 ESN configurations across 3,600 time
series and uses a multisession parallel plan.

### 3. Evaluate random initialization

The scripts in `random/` examine how forecasts and forecast accuracy
vary across different random reservoir initializations.

The default random seed is included as a reference, together with
additional randomly generated seeds. The resulting scripts reproduce the
random-seed accuracy summaries and the monthly and quarterly forecast
illustration used in the paper.

### 4. Run the forecast benchmark

The main entry point for the benchmark is `forecast/fcst_script_main.R`.
Monthly and quarterly data are processed in separate runs:

1.  In `fcst_script_main.R`, source exactly one configuration file:
    `fcst_config_m4_mth.R` or `fcst_config_m4_qtr.R`.
2.  In the selected configuration file, confirm the input data, forecast
    horizon, ESN parameters, model list, and execution settings.
3.  Set `test_run <- FALSE` for the complete sample.
4.  Run `fcst_script_main.R`.
5.  Repeat the process using the configuration for the other frequency.

The main script executes the preprocessing, train-test split, model
estimation, forecast generation, accuracy calculation, runtime
collection, and post-processing scripts in numerical order.

Each run creates a dated directory under `forecast/` containing:

- log files;
- forecasts and forecast errors;
- accuracy and runtime objects;
- intermediate results;
- a saved workspace for the reporting scripts.

These directories are working files. Retain the completed monthly and
quarterly workspaces required for the final output step.

### 5. Create the benchmark outputs

Open `forecast/fcst_600_create_output.R` and specify:

- the frequency using `set_freq`;
- the completed run using `file_workspace`.

Run the script once for the monthly results and once for the quarterly
results. It calculates the MASE, sMAPE, OWA, and runtime summaries and
creates the final tables and figures used in the paper.

The curated output objects belong in `output/`.

## Reproducibility

The complete hyperparameter sweep and forecast benchmark are
computationally intensive and use parallel processing. Results may
therefore depend on the available hardware, the number of workers, and
the installed package versions.
