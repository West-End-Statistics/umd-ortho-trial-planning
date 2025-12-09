#' Run Bayesian Interim Analysis for Simulated Trial Data
#'
#' Simulates a clinical trial and performs Bayesian analysis at multiple interim
#' analysis timepoints using a Stan model. Can return either frequentist estimates
#' or full Bayesian posterior samples.
#'
#' @param n_per_arm Integer. Number of subjects per arm in the full trial.
#' @param data_cuts Numeric vector. Sample sizes for interim analyses (e.g., c(50, 100, 150)).
#' @param mod CmdStan model object. Compiled Stan model from \code{\link{create_cmdstan_model}}.
#' @param ... Additional arguments passed to \code{\link{simulate_trial}}.
#' @param seed Integer. Random seed for reproducibility (default: NULL).
#' @param alpha Numeric. Significance level for frequentist estimates (default: 0.05).
#' @param prior_std Numeric. Standard deviation of prior distribution (default: 1).
#' @param freq_outputs Logical. If TRUE, returns frequentist estimates only; if FALSE,
#'   returns Bayesian posterior samples (default: FALSE).
#'
#' @return If freq_outputs = TRUE, list of frequentist estimates for each data cut.
#'   If freq_outputs = FALSE, list of CmdStan fit objects with posterior samples.
#'
#' @export
#' @importFrom purrr map
bayesian_trial_raw_results <- function(
  n_per_arm, data_cuts, mod, ..., seed = NULL, alpha = .05,
  prior_std = 1,
  freq_outputs = FALSE
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  all_trial_data <- simulate_trial(n_per_arm = n_per_arm, ...)
  data_subsets <- purrr::map(data_cuts, subset_trial, d = all_trial_data)


  if (freq_outputs) {
    freq_estimates <- data_subsets |>
      purrr::map(estimate_dataset) |>
      purrr::map(function(x) subset(x, term == "days_at_home_t7"))
    return(freq_estimates)
  } else {
    freq_estimates <- data_subsets |>
      purrr::map(estimate_dataset, alpha = alpha) |>
      purrr::map(function(x) subset(x, term == "days_at_home_t7", select = c(estimate, std.error)))
  }

  trial_output <- purrr::map(freq_estimates, function(x) {
    fit <- mod$sample(
      data = make_stan_data(x, prior_std = prior_std),
      # seed = 123,
      chains = 1,
      parallel_chains = 1,
      refresh = 0,
      show_messages = FALSE
    )
    fit
    # stan_success(fit)
  })

  trial_output
}
