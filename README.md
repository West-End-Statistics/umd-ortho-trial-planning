# Orthopedic Trial Planing




## Running Interim Analysis

This describes the requirements for re-running the code in "vignettes/interim-analysis.qmd"

### Dependencies

1. Install cmdstanr and cmdstan [Source](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)

```r
# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
library(cmdstanr)
install_cmdstan(cores = 2)
```

You can then install the `umdorthotrialplanning` package with the `remotes` package:

```r
remotes::install_github("West-End-Statistics/umd-ortho-trial-planning@vitallish/issue10")
```

## Orthopedic Trial Planning Application


## Changes

### 2025-11-13

- moved app to inst/ folder for later use

### 2024-09-25
- Added ability to add missing outcomes to data - % missing per outcome.


### 2024-09-09
- Statistics Parameters
  - Added ability to have one or two-sided alpha. When using one-sided you can specify whether to test against control or active arm.
  - Ability to modify alpha

- Power Analysis
  - Split out components of win ratio
  - Added average estimate for each model including average winratio.

- User interface updates
  - Settings and output in various tabs to allow for easier setting
  - Buttons that run simulations etc. moved to main panel to tie in more closely with outputs
- Data simulation
  - Updated simulations to use NORTA (normal to anything) strategy.