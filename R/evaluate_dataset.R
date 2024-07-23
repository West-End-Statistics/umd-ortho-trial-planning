evaluate_dataset <- function(d,
                             alpha = .05,
                             amb_status_thresh = 1,
                             days_at_home_thresh = 7) {
  bt_out <- BuyseTest(
    arm ~ bin(died, operator = "<0") +
      cont(amb_status_numeric, threshold = amb_status_thresh) +
      cont(days_at_home, threshold = days_at_home_thresh),
    method.inference = "u-statistic",
    trace = 0,
    data = d
  )
  winratio_out <- tibble::as_tibble(confint(bt_out, statistic = "winRatio"), rownames = "term")


  winratio_summary <- winratio_out |>
    subset(grepl("days_at_home", term))

  winratio_win <- winratio_summary$estimate > 1 && winratio_summary$p.value < alpha

  death_summary <- glm(died ~ arm, data = d, family = "binomial") |>
    broom::tidy() |>
    subset(term == "armactive")

  death_win <- death_summary$estimate < 0 && death_summary$p.value < alpha

  amb_status_summary <- MASS::polr(ambulation_status ~ arm,
    data = d,
    Hess = TRUE
  ) |>
    broom::tidy(p.values = TRUE) |>
    subset(term == "armactive")
  amb_status_win <- amb_status_summary$estimate > 0 && amb_status_summary$p.value < alpha

  days_summary <- lm(days_at_home ~ arm, data = d) |>
    broom::tidy() |>
    subset(term == "armactive")

  days_win <- days_summary$estimate > 0 && days_summary$p.value < alpha
  data.frame(
    winratio = winratio_win,
    death = death_win,
    amb_status = amb_status_win,
    days = days_win
  )
}
