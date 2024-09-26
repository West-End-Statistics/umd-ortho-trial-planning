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
