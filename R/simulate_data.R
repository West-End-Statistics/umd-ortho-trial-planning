#' Simulate Clinical Trial Data Using NORTA Methodology
#'
#' Generates synthetic patient data for a single trial arm using the NORTA
#' (Normal-to-Anything) correlation structure. Simulates hierarchical outcomes
#' including survival, days at home, and ambulatory status with configurable
#' correlation and missingness.
#'
#' @param n Integer. Number of subjects to simulate.
#' @param arm Character. Trial arm, either "placebo" or "active".
#' @param outcome_correlation Numeric. Correlation coefficient between outcomes (default: 0).
#' @param survival_prob Numeric vector of length 2. Survival probabilities for
#'   placebo and active arms (default: c(0.85, 0.90)).
#' @param days_at_home_mean Numeric vector of length 2. Mean days at home for
#'   placebo and active arms (default: c(67, 78)).
#' @param days_at_home_sd Numeric vector of length 2. Standard deviation of days
#'   at home for both arms (default: c(20, 20)).
#' @param placebo_amb_status Numeric vector of length 3. Ambulatory status proportions
#'   for placebo arm (default: c(0.07, 0.37, 0.34)).
#' @param amb_status_latent_shift Numeric. Latent shift for ambulatory status in
#'   active arm (default: 0.2).
#' @param survival_missing Numeric. Proportion of missing survival data (default: 0).
#' @param days_at_home_missing Numeric. Proportion of missing days at home data (default: 0).
#' @param amb_status_missing Numeric. Proportion of missing ambulatory status data (default: 0).
#'
#' @return A data frame with columns:
#'   \item{died}{Binary indicator of death (0 = survived, 1 = died)}
#'   \item{days_at_home}{Integer days at home (1-121)}
#'   \item{ambulation_status}{Ordered factor of ambulatory status}
#'   \item{amb_status_numeric}{Numeric version of ambulatory status}
#'
#' @export
#' @importFrom mvtnorm rmvnorm
#'
#' @examples
#' # Simulate 100 placebo patients
#' placebo_data <- simulate_data(n = 100, arm = "placebo")
#'
#' # Simulate 100 active arm patients with correlation
#' active_data <- simulate_data(n = 100, arm = "active", outcome_correlation = 0.3)
simulate_data <- function(
    n,
    arm = c("placebo", "active"),
    outcome_correlation = 0,
    survival_prob = c(.85, .90),
    days_at_home_mean = c(67, 78),
    days_at_home_sd = c(20, 20),
    placebo_amb_status = c(.07, .37, .34),
    amb_status_latent_shift = .2,
    survival_missing = 0,
    days_at_home_missing = 0,
    amb_status_missing = 0) {
  arm <- match.arg(arm)

  sds <- matrix(1, ncol = 1, nrow = 3)
  covar <- sds %*% t(sds)
  non_diag_sel <- lower.tri(covar) | upper.tri(covar)
  covar[non_diag_sel] <- covar[non_diag_sel] * outcome_correlation
  if (arm == "placebo") {
    prob_surv <- survival_prob[1]
    d_at_home <- days_at_home_mean[1]
    d_at_home_sd <- days_at_home_sd[1]
    latent_walk <- 0
  } else {
    prob_surv <- survival_prob[2]
    d_at_home <- days_at_home_mean[2]
    d_at_home_sd <- days_at_home_sd[2]
    latent_walk <- amb_status_latent_shift
  }

  underlying_data <- mvtnorm::rmvnorm(
    n = n,
    c(
      0, 0, 0
    ),
    covar
  )

  to_prob <- pnorm(underlying_data)
  out <- data.frame(
    died = qbinom(to_prob[, 1], prob = 1 - prob_surv, size = 1),
    days_at_home = qnorm(to_prob[, 2], mean = d_at_home, sd = d_at_home_sd),
    ambulation_status = qnorm(to_prob[, 3], mean = latent_walk, sd = 1)
  )

  out$days_at_home <- round(pmax(pmin(out$days_at_home, 121), 1))
  out$ambulation_status <- get_amb_status(
    out$ambulation_status,
    placebo_amb_status[1],
    placebo_amb_status[2],
    placebo_amb_status[3]
  )

  missing_index <- sample(1:n, size = round(survival_missing * n))
  out$died[missing_index] <- NA

  missing_index <- sample(1:n, size = round(days_at_home_missing * n))
  out$days_at_home[missing_index] <- NA

  missing_index <- sample(1:n, size = round(amb_status_missing * n))
  out$ambulation_status[missing_index] <- NA

  out$amb_status_numeric <- as.numeric(out$ambulation_status)
  out
}

#' Create Custom Outcome Correlation Matrix
#'
#' Helper function to create a correlation vector for outcomes.
#'
#' @param died_daysathome Numeric. Correlation between death and days at home (default: 0).
#' @param died_ambulation Numeric. Correlation between death and ambulation (default: 0).
#' @param days_ambulation Numeric. Correlation between days at home and ambulation (default: 0).
#'
#' @return Numeric vector of length 6 with correlation values.
#' @export
custom_outcome_corr <- function(died_daysathome = 0, died_ambulation = 0, days_ambulation = 0) {
  c(
    died_daysathome,
    died_ambulation,
    died_daysathome,
    days_ambulation,
    died_ambulation,
    days_ambulation
  )
}
