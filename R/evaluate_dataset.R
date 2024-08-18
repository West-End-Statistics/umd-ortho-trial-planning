estimate_dataset <- function(d,
                             alpha = .05,
                             amb_status_thresh = 1,
                             days_at_home_thresh = 7,
                             two_sided = TRUE) {
  confint_width <- ifelse(two_sided,
    1 - alpha,
    1 - alpha * 2
  )
  old_options <- BuyseTest.options()
  BuyseTest.options(
    conf.level = confint_width
  )

  on.exit(
    do.call(BuyseTest.options, args = old_options)
  )
  bt_out <- BuyseTest(
    arm ~ bin(died, operator = "<0") +
      cont(amb_status_numeric, threshold = amb_status_thresh) +
      cont(days_at_home, threshold = days_at_home_thresh),
    method.inference = "u-statistic",
    trace = 0,
    data = d
  )

  winratio_out <- tibble::as_tibble(
    confint(bt_out, statistic = "winRatio", level = confint_width),
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
  data.frame(
    winratio = winratio_win,
    death = death_win,
    amb_status = amb_status_win,
    days = days_win
  )



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
