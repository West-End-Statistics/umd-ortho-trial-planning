#' Evaluate Bayesian Trial Success Criteria
#'
#' Calculates posterior probabilities from Bayesian model fit and determines if
#' trial meets predefined success criteria for superiority, inferiority, or equivalence.
#'
#' @param fit CmdStan fit object. Posterior samples from Bayesian model.
#' @param pr_superiority_treatment_benefit Numeric. Threshold probability for treatment
#'   benefit superiority (default: 0.99).
#' @param pr_inferiority_treatment_benefit Numeric. Threshold probability for treatment
#'   benefit inferiority (default: 0.01).
#' @param pr_superiority_clinical_benefit Numeric. Threshold probability for clinically
#'   meaningful benefit (default: 0.9).
#' @param pr_equivalence Numeric. Threshold probability for equivalence (default: 0.9).
#' @param pr_treat_equiv_bounds Numeric vector of length 2. Posterior probability bounds
#'   for treatment equivalence (default: c(1/3, 2/3)).
#' @param wr_treat_equiv_bounds Numeric vector of length 2. Win ratio bounds for
#'   equivalence region (default: c(0.8, 1.25)).
#' @param wr_clinical_benefit Numeric. Win ratio threshold for clinically meaningful
#'   benefit (default: 1.5).
#'
#' @return Data frame with logical columns for each stopping rule and estimated win ratio:
#'   \item{superiority_treatment_benefit}{Treatment is superior}
#'   \item{superiority_clinical_benefit}{Clinically meaningful benefit}
#'   \item{inferiority_treatment_benefit}{Treatment is inferior}
#'   \item{equivalence}{Treatment is equivalent}
#'   \item{equiv_superior}{Equivalent and trending superior}
#'   \item{equiv_inferior}{Equivalent and trending inferior}
#'   \item{win_ratio}{Posterior mean win ratio}
#'
#' @export
trial_success <- function(
  fit,
  pr_superiority_treatment_benefit = .99,
  pr_inferiority_treatment_benefit = 1 - pr_superiority_treatment_benefit,
  pr_superiority_clinical_benefit = .9,
  pr_equivalence = 0.9,
  pr_treat_equiv_bounds = c(1 / 3, 2 / 3),
  wr_treat_equiv_bounds = c(.8, 1.25),
  wr_clinical_benefit = 1.5
) {
  x <- fit$summary("theta",
    pr_treatment_benefit = ~ mean(. > 1),
    pr_clinical_benefit = ~ mean(. > wr_clinical_benefit),
    pr_equivalence = ~ mean(. > wr_treat_equiv_bounds[1] &
      . < wr_treat_equiv_bounds[2])
  )

  out <- data.frame(
    superiority_treatment_benefit =
      x$pr_treatment_benefit > pr_superiority_treatment_benefit,
    superiority_clinical_benefit =
      x$pr_clinical_benefit > pr_superiority_clinical_benefit,
    inferiority_treatment_benefit =
      x$pr_treatment_benefit < pr_inferiority_treatment_benefit,
    equivalence =
      x$pr_equivalence > pr_equivalence &
        x$pr_treatment_benefit > pr_treat_equiv_bounds[1] &
        x$pr_treatment_benefit < pr_treat_equiv_bounds[2],
    equiv_superior = x$pr_equivalence > pr_equivalence &
      x$pr_treatment_benefit > pr_treat_equiv_bounds[2],
    equiv_inferior = x$pr_equivalence > pr_equivalence &
      x$pr_treatment_benefit < pr_treat_equiv_bounds[1]
  )

  out$win_ratio <- fit$summary(variables = "theta")$mean

  out
}


#' Subset Trial Data for Interim Analysis
#'
#' Extracts a balanced subset of subjects from each arm for interim analysis.
#'
#' @param n_per_arm Integer. Number of subjects per arm to include (default: 50).
#' @param d Data frame. Full trial dataset with both arms.
#'
#' @return Data frame with n_per_arm subjects from each arm.
#'
#' @keywords internal
subset_trial <- function(n_per_arm = 50, d) {
  total_subj <- nrow(d)
  if (n_per_arm * 2 > total_subj) {
    stop("Cannot subset more than exists")
  }

  part_1 <- 1:n_per_arm
  part_2 <- part_1 + total_subj / 2
  d[c(part_1, part_2), ]
}

#' Summarize Trial Simulation Results
#'
#' Aggregates results across multiple trial simulations, tracking cumulative
#' stopping rules and average win ratios at each interim analysis.
#'
#' @param x Data frame. Trial results with columns: .sim, analysis_amount, stopping
#'   rules, and win_ratio.
#'
#' @return Data frame with mean success rates for each stopping rule by analysis timepoint.
#'
#' @export
#' @importFrom dplyr mutate group_by summarize across
summarize_results <- function(x) {
  x |>
    mutate(analysis_amount = as.numeric(analysis_amount)) |>
    group_by(.sim) |>
    # don't mutate over win ratio - just fill down if there is a 1 in anything
    # .sim is automatically excluded
    mutate(across(-c(win_ratio, analysis_amount), function(x) cumsum(x) > 0L)) |>
    group_by(analysis_amount) |>
    summarize(across(-.sim, mean))
}

#' Determine Trial Stopping Point and Reason
#'
#' Identifies when a trial would stop based on which success criterion is met first,
#' or if it continues to completion without reaching any stopping rule.
#'
#' @param x Data frame. Trial results with stopping rule indicators and win ratios.
#'
#' @return Data frame with columns:
#'   \item{stopping_rule}{Name of stopping rule triggered or "No Conclusion"}
#'   \item{stopping_subjects}{Number of subjects at stopping point}
#'   \item{stopping_win_ratio}{Win ratio estimate at stopping point}
#'
#' @export
compute_trial_stop <- function(x) {
  col_to_check <- c(
    "superiority_treatment_benefit",
    "superiority_clinical_benefit",
    "inferiority_treatment_benefit",
    "equivalence",
    "equiv_superior",
    "equiv_inferior"
  )

  all_stops <- sapply(x[, col_to_check], function(y) {
    # find the first stop in each column
    x$analysis_amount[which(y)[1]]
  })

  # which column stopped first
  trial_result <- all_stops[which.min(all_stops)]


  if (length(trial_result) != 1) {
    out <- data.frame(
      stopping_rule = "No Conclusion",
      stopping_subjects = max(x$analysis_amount),
      stopping_win_ratio = tail(x$win_ratio, 1)
    )
  } else {
    win_ratio <- which(unname(trial_result) == x$analysis_amount)
    win_ratio <- x$win_ratio[win_ratio]

    out <- data.frame(
      stopping_rule = names(trial_result),
      stopping_subjects = unname(trial_result),
      stopping_win_ratio = win_ratio
    )
  }
  out
}

#' Create Summary Table of Trial Stopping Rules
#'
#' Processes simulation results to create a formatted flextable showing statistics
#' for each stopping rule, including total stops, average sample size, and percentiles.
#'
#' @param sim_results Data frame. Simulation results with .sim identifier and trial
#'   outcomes from \code{\link{trial_success}}.
#'
#' @return A flextable object with summary statistics by stopping rule including
#'   an "Overall" row aggregating all results.
#'
#' @export
#' @importFrom dplyr bind_rows mutate group_by summarise rename
#' @importFrom purrr map list_rbind
#' @importFrom forcats fct_relevel
#' @importFrom flextable flextable set_header_labels
summarize_stopping_rules <- function(sim_results) {
  all_res_temp <- sim_results |>
    split(~.sim) |>
    purrr::map(compute_trial_stop) |>
    purrr::list_rbind() |>
    dplyr::rename(rule = stopping_rule)

  all_res_temp <-
    dplyr::bind_rows(
      all_res_temp,
      all_res_temp |> dplyr::mutate(rule = "Overall")
    ) |>
    dplyr::mutate(
      rule = factor(rule),
      rule = forcats::fct_relevel(rule, "Overall", after = Inf)
    )

  all_res_temp |>
    dplyr::group_by(rule) |>
    dplyr::summarise(
      total_stops = n(),
      average_size = mean(stopping_subjects),
      average_win_ratio = mean(stopping_win_ratio),
      trials_stopped_by_90 = quantile(stopping_subjects, .9),
      trials_stopped_by_95 = quantile(stopping_subjects, .95),
      .groups = "drop"
    ) |>
    flextable::flextable() |>
    flextable::set_header_labels(
      group = "Group",
      rule = "Rule",
      total_stops = "Total Stops",
      average_size = "Average Size",
      average_win_ratio = "Average Win Ratio",
      trials_stopped_by_90 = "90% of Trials Stopped",
      trials_stopped_by_95 = "95% of Trials Stopped"
    )
}

#' Plot Trial Stopping Points by Rule
#'
#' Creates a scatter plot showing the relationship between stopping sample size
#' and win ratio for each stopping rule across simulations.
#'
#' @param sim_results Data frame. Simulation results with .sim identifier and trial
#'   outcomes from \code{\link{trial_success}}.
#'
#' @return A ggplot2 object showing stopping subjects vs. win ratio colored by rule.
#'
#' @export
#' @importFrom dplyr bind_rows mutate rename
#' @importFrom purrr map list_rbind
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 ggplot aes geom_point theme_bw
plot_stopping_rules <- function(sim_results) {
  all_res_temp <- sim_results |>
    split(~.sim) |>
    purrr::map(compute_trial_stop) |>
    purrr::list_rbind() |>
    dplyr::rename(rule = stopping_rule)

  all_res_temp <-
    dplyr::bind_rows(
      all_res_temp,
      all_res_temp |> dplyr::mutate(rule = "Overall")
    ) |>
    dplyr::mutate(
      rule = factor(rule),
      rule = forcats::fct_relevel(rule, "Overall", after = Inf)
    )

  all_res_temp |>
    ggplot2::ggplot(ggplot2::aes(
      color = rule,
      x = stopping_subjects,
      y = stopping_win_ratio
    )) +
    ggplot2::geom_point(alpha = .5) +
    ggplot2::theme_bw()
}

#' Create Alluvial Diagram of Trial Stopping Decisions
#'
#' Generates an alluvial (flow) diagram showing how trial stopping decisions
#' evolve across interim analysis timepoints. Shows the flow of simulations
#' between "No Conclusion" and various stopping rules as sample size increases.
#'
#' @param sim_results Data frame. Simulation results with .sim identifier and trial
#'   outcomes from \code{\link{trial_success}}.
#' @param data_cuts Numeric vector. Sample sizes at which interim analyses occur.
#'
#' @return A ggplot2 object with alluvial diagram showing decision flows across
#'   interim analyses.
#'
#' @export
#' @importFrom dplyr select group_by filter mutate ungroup summarise across
#' @importFrom purrr map list_rbind
#' @importFrom tidyr expand_grid pivot_wider
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 ggplot aes theme_minimal
#' @importFrom ggalluvial to_lodes_form geom_alluvium geom_stratum
plot_stopping_alluvial <- function(sim_results, data_cuts) {
  alluvial_df <- sim_results |>
    split(~.sim) |>
    purrr::map(compute_trial_stop) |>
    purrr::list_rbind(names_to = ".sim") |>
    dplyr::select(-stopping_win_ratio) |>
    tidyr::expand_grid(all_cuts = data_cuts) |>
    dplyr::group_by(.sim) |>
    dplyr::filter(all_cuts <= stopping_subjects) |>
    dplyr::mutate(stopping_rule = if_else(
      all_cuts < stopping_subjects,
      "No Conclusion",
      stopping_rule
    )) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      stopping_rule = factor(stopping_rule),
      stopping_rule = forcats::fct_relevel(stopping_rule, "No Conclusion")
    ) |>
    dplyr::select(-stopping_subjects) |>
    tidyr::pivot_wider(names_from = all_cuts, values_from = stopping_rule) |>
    dplyr::group_by(dplyr::across(-.sim)) |>
    dplyr::summarise(Freq = n(), .groups = "drop")

  alluvial_df |>
    ggalluvial::to_lodes_form(
      key = "Trial Size",
      axes = seq_len(ncol(alluvial_df) - 1)
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = `Trial Size`,
      stratum = stratum,
      alluvium = alluvium,
      label = stratum,
      y = Freq
    ) +
    ggalluvial::geom_alluvium(fill = "lightblue") +
    ggalluvial::geom_stratum(ggplot2::aes(fill = stratum)) +
    ggplot2::theme_minimal()
}
