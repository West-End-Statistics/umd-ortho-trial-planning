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
                               cores = 2,
                               return_fit = FALSE) {
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

  if (return_fit) {
    return(fit)
  }
  # Extract results
  draws <- fit$draws(variables = c("wr"))

  # Create output similar to BuyseTest confint
  alpha <- 1 - level

  # Extract global win ratio
  wr_global <- as_draws_matrix(draws)[, "wr"]

  # Create results dataframe with only global win ratio
  results <- data.frame(
    estimate = mean(wr_global),
    se = sd(wr_global),
    lower.ci = quantile(wr_global, alpha / 2),
    upper.ci = quantile(wr_global, 1 - alpha / 2),
    null = 1,
    p.value = mean(wr_global <= 1)
  )

  rownames(results) <- c("global")

  # Add attributes
  attr(results, "method") <- "Bayesian"
  attr(results, "level") <- level
  attr(results, "chains") <- chains
  attr(results, "fit") <- fit

  return(results)
}

# Example usage
set.seed(123)
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
bayesian_out_model <- bayesian_win_ratio(d, cores = 2, return_fit = TRUE)

# Compare results
print("Classical Win Ratio Results:")
print(classical_out)
print("\nBayesian Global Win Ratio Results:")
print(bayesian_out)
