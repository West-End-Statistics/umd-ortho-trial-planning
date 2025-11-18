#' Estimate Win Ratio and Component Outcomes for Trial Dataset
#'
#' Calculates win ratio using BuyseTest with hierarchical endpoints (death,
#' ambulatory status, days at home) along with univariate analyses.
#'
#' @param d Data frame. Trial dataset with columns: arm, died, amb_status_numeric,
#'   days_at_home, ambulation_status.
#' @param alpha Numeric. Significance level (default: 0.05).
#' @param amb_status_thresh Numeric. Threshold for ambulatory status (default: 1).
#' @param days_at_home_thresh Numeric. Threshold for days at home (default: 7).
#' @param two_sided Logical. Whether to use two-sided tests (default: TRUE).
#'
#' @return Data frame with win ratio estimates, standard errors, and confidence intervals
#'   for each outcome and the overall win ratio.
#'
#' @export
#' @importFrom BuyseTest BuyseTest.options BuyseTest confint
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename bind_rows
#' @importFrom broom tidy
#' @importFrom MASS polr
estimate_dataset <- function(d,
                             alpha = .05,
                             amb_status_thresh = 1,
                             days_at_home_thresh = 7,
                             two_sided = TRUE) {
  confint_width <- ifelse(two_sided,
    1 - alpha,
    1 - alpha * 2
  )
  old_options <- BuyseTest::BuyseTest.options()
  BuyseTest::BuyseTest.options(
    conf.level = confint_width
    # order.Hprojection = 2
  )

  on.exit(
    do.call(BuyseTest::BuyseTest.options, args = old_options)
  )
  bt_out <- BuyseTest::BuyseTest(
    arm ~ bin(died, operator = "<0") +
      cont(amb_status_numeric, threshold = amb_status_thresh) +
      cont(days_at_home, threshold = days_at_home_thresh),
    # method.inference = "varExact permutation",
    trace = 0,
    data = d,
    cpus = 2
  )

  winratio_out <- tibble::as_tibble(
    BuyseTest::confint(bt_out, statistic = "winRatio", level = confint_width),
    rownames = "term"
  )
  winratio_out$null <- NULL
  winratio_out <- dplyr::rename(winratio_out,
    std.error = se,
    conf.low = lower.ci,
    conf.high = upper.ci
  )


  winratio_summary <- winratio_out |>
    subset(grepl("days_at_home", term))


  # switch(alpha_sided,
  # "two" = winratio_summary$p.value < alpha,
  # "treatment" = winratio_summary$estimate > 1 && winratio_summary$p.value < alpha * 2
  # )
  # "control" <- winratio_summary$estimate > 1 && winratio_summary$p.value < alpha * 2

  # winratio_win <- winratio_summary$estimate > 1 && winratio_summary$p.value < alpha

  death_summary <- glm((1 - died) ~ arm, data = d, family = "binomial") |>
    broom::tidy(conf.int = TRUE, conf.level = confint_width) |>
    subset(term == "armactive")

  # death_win <- death_summary$estimate > 0 && death_summary$p.value < alpha

  amb_status_summary <- MASS::polr(ambulation_status ~ arm,
    data = d,
    Hess = TRUE
  ) |>
    broom::tidy(p.values = TRUE, conf.int = TRUE, conf.level = confint_width) |>
    subset(term == "armactive")
  # amb_status_win <- amb_status_summary$estimate > 0 && amb_status_summary$p.value < alpha

  days_summary <- lm(days_at_home ~ arm, data = d) |>
    broom::tidy(conf.int = TRUE, conf.level = confint_width) |>
    subset(term == "armactive")

  # days_win <- days_summary$estimate > 0 && days_summary$p.value < alpha
  # data.frame(
  #   winratio = winratio_win,
  #   death = death_win,
  #   amb_status = amb_status_win,
  #   days = days_win
  # )



  out <- dplyr::bind_rows(
    winratio = winratio_out,
    death = death_summary,
    ambulation_status = amb_status_summary,
    days_at_home = days_summary,
    .id = "model"
  )
  out$statistic <- NULL
  out$coef.type <- NULL
  out
}


#' Check if Trial Results Meet Success Criteria
#'
#' Determines which outcomes meet statistical significance based on p-values and
#' effect direction relative to comparison thresholds.
#'
#' @param tidy_estimates Data frame. Tidied estimates from \code{\link{estimate_dataset}}.
#' @param alpha Numeric. Significance level (default: 0.05).
#' @param alpha_comparison Character. Type of comparison: "both" (two-sided),
#'   "treatment" (one-sided favoring treatment), or "control" (one-sided favoring control).
#' @param add_to_df Logical. If TRUE, adds "win" column to data frame; if FALSE,
#'   returns named logical vector (default: TRUE).
#'
#' @return Either data frame with added "win" column or named logical vector
#'   indicating which outcomes met success criteria.
#'
#' @keywords internal
check_if_successful <- function(
    tidy_estimates,
    alpha = .05,
    alpha_comparison = c("both", "treatment", "control"),
    add_to_df = TRUE) {
  alpha_comparison <- match.arg(alpha_comparison)
  alpha_to_compare <- switch(alpha_comparison,
    "both" = alpha,
    alpha * 2
  )
  estimate_comparisons <- c(
    # win ratio comparisons
    1, 1, 1,
    # estimate from glm on log scale
    0, 0, 0
  )

  winners <- switch(alpha_comparison,
    "both" = tidy_estimates$p.value < alpha_to_compare,
    "treatment" = with(
      tidy_estimates,
      (estimate > estimate_comparisons) & (p.value < alpha_to_compare)
    ),
    "control" = with(
      tidy_estimates,
      (estimate < estimate_comparisons) & (p.value < alpha_to_compare)
    )
  )
  if (add_to_df) {
    out <- tidy_estimates
    out[["win"]] <- winners
  } else {
    out <- winners
    names(out) <- c(
      "winratio_death",
      "winratio_amb_status",
      "winratio_days_at_home",
      "uni_death",
      "uni_amb_status",
      "uni_days_at_home"
    )
  }
  out
}

#' Evaluate Trial Dataset for Treatment Success
#'
#' Performs win ratio analysis and determines if treatment success criteria are met
#' based on specified alpha level and comparison type.
#'
#' @param d Data frame. Trial dataset.
#' @param alpha Numeric. Significance level (default: 0.05).
#' @param amb_status_thresh Numeric. Threshold for ambulatory status (default: 1).
#' @param days_at_home_thresh Numeric. Threshold for days at home (default: 7).
#' @param alpha_comparison Character. Type of comparison: "both" (two-sided),
#'   "treatment" (one-sided favoring treatment), or "control" (one-sided favoring control).
#' @param include_estimates Logical. Whether to include full estimates (default: TRUE).
#'
#' @return Data frame with success indicators and optionally full estimates.
#'
#' @export
evaluate_dataset <- function(
    d,
    alpha = .05,
    amb_status_thresh = 1,
    days_at_home_thresh = 7,
    alpha_comparison = c("both", "treatment", "control"),
    include_estimates = TRUE) {
  alpha_comparison <- match.arg(alpha_comparison)
  two_sided <- alpha_comparison == "both"


  tidy_estimates <- estimate_dataset(d,
    alpha = alpha, amb_status_thresh = amb_status_thresh,
    days_at_home_thresh = days_at_home_thresh,
    two_sided = two_sided
  )

  check_if_successful(
    tidy_estimates = tidy_estimates,
    alpha = alpha,
    alpha_comparison = alpha_comparison,
    add_to_df = include_estimates
  )
}
