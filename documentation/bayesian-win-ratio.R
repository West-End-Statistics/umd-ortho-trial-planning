# ---
# title: "Bayesian Win Ratio Implementation"
# date: today
# format:
#   html:
#     embed-resources: true
# ---

library(BuyseTest)
library(cmdstanr)
library(posterior)
r_files <- list.files(here::here("R"), full.names = TRUE)
sapply(r_files, source)

# Bayesian Win Ratio Implementation
bayesian_win_ratio <- function(data,
                               amb_threshold = 1,
                               days_threshold = 7,
                               prior_mean_log_wr = 0,
                               prior_sd_log_wr = 2,
                               level = 0.95,
                               chains = 4,
                               iter = 2000,
                               warmup = 1000,
                               cores = 2) {
  # Split data by treatment arm
  control_data <- data[data$arm == "placebo", ]
  treatment_data <- data[data$arm == "active", ]

  # Prepare Stan data using raw data
  stan_data <- list(
    n_control = nrow(control_data),
    n_treatment = nrow(treatment_data),
    death_control = control_data$died,
    death_treatment = treatment_data$died,
    amb_control = control_data$amb_status_numeric,
    amb_treatment = treatment_data$amb_status_numeric,
    amb_threshold = amb_threshold,
    days_control = control_data$days_at_home,
    days_treatment = treatment_data$days_at_home,
    days_threshold = days_threshold,
    prior_mean_log_wr = prior_mean_log_wr,
    prior_sd_log_wr = prior_sd_log_wr
  )

  # Compile and fit Stan model
  stan_file <- file.path(here::here("inst", "stanmod", "bayesian_win_ratio.stan"))
  model <- cmdstan_model(stan_file)

  fit <- model$sample(
    data = stan_data,
    chains = chains,
    iter_warmup = warmup,
    iter_sampling = iter - warmup,
    parallel_chains = min(chains, cores),
    refresh = 0
  )

  # Extract results
  draws <- fit$draws(variables = c("wr", "wr_amb_hier", "wr_days_hier", "wr_death", "wr_amb", "wr_days"))

  # Create output similar to BuyseTest confint
  alpha <- 1 - level

  # Extract win ratios for each endpoint
  wr_global <- as_draws_matrix(draws)[, "wr"]
  wr_amb_hier <- as_draws_matrix(draws)[, "wr_amb_hier"]
  wr_days_hier <- as_draws_matrix(draws)[, "wr_days_hier"]
  wr_death <- as_draws_matrix(draws)[, "wr_death"]
  wr_amb <- as_draws_matrix(draws)[, "wr_amb"]
  wr_days <- as_draws_matrix(draws)[, "wr_days"]

  # Create results dataframe
  results <- data.frame(
    estimate = c(mean(wr_death), mean(wr_amb), mean(wr_days)),
    se = c(sd(wr_death), sd(wr_amb), sd(wr_days)),
    lower.ci = c(
      quantile(wr_death, alpha / 2),
      quantile(wr_amb, alpha / 2),
      quantile(wr_days, alpha / 2)
    ),
    upper.ci = c(
      quantile(wr_death, 1 - alpha / 2),
      quantile(wr_amb, 1 - alpha / 2),
      quantile(wr_days, 1 - alpha / 2)
    ),
    null = c(1, 1, 1),
    p.value = c(
      mean(wr_death <= 1),
      mean(wr_amb <= 1),
      mean(wr_days <= 1)
    )
  )

  rownames(results) <- c("died", "amb_status_numeric_t1", "days_at_home_t7")

  # Add attributes
  attr(results, "method") <- "Bayesian"
  attr(results, "level") <- level
  attr(results, "chains") <- chains
  attr(results, "fit") <- fit
  attr(results, "global_wr") <- list(
    estimate = mean(wr_global),
    se = sd(wr_global),
    lower.ci = quantile(wr_global, alpha / 2),
    upper.ci = quantile(wr_global, 1 - alpha / 2),
    p.value = mean(wr_global <= 1)
  )
  attr(results, "amb_hier_wr") <- list(
    estimate = mean(wr_amb_hier),
    se = sd(wr_amb_hier),
    lower.ci = quantile(wr_amb_hier, alpha / 2),
    upper.ci = quantile(wr_amb_hier, 1 - alpha / 2),
    p.value = mean(wr_amb_hier <= 1)
  )
  attr(results, "days_hier_wr") <- list(
    estimate = mean(wr_days_hier),
    se = sd(wr_days_hier),
    lower.ci = quantile(wr_days_hier, alpha / 2),
    upper.ci = quantile(wr_days_hier, 1 - alpha / 2),
    p.value = mean(wr_days_hier <= 1)
  )

  return(results)
}

# Example usage
d <- simulate_trial(n_per_arm = 100)

# Classical BuyseTest approach
bt_out <- BuyseTest(
  arm ~ bin(died, operator = "<0") +
    cont(amb_status_numeric, threshold = 1) +
    cont(days_at_home, threshold = 7),
  trace = 0,
  data = d,
  cpus = 2
)

classical_out <- confint(bt_out, statistic = "winRatio", level = .95)

# Bayesian approach
bayesian_out <- bayesian_win_ratio(d, cores = 2)

# Compare results
print("Classical Win Ratio Results:")
print(classical_out)
print("\nBayesian Win Ratio Results:")
print(bayesian_out)
print("\nBayesian Global Win Ratio (death → amb → days):")
print(attr(bayesian_out, "global_wr"))
print("\nBayesian Ambulation Hierarchical Win Ratio (death → amb):")
print(attr(bayesian_out, "amb_hier_wr"))
print("\nBayesian Days Hierarchical Win Ratio (amb → days):")
print(attr(bayesian_out, "days_hier_wr"))
