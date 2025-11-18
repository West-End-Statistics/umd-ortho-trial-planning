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


subset_trial <- function(n_per_arm = 50, d) {
  total_subj <- nrow(d)
  if (n_per_arm * 2 > total_subj) {
    stop("Cannot subset more than exists")
  }

  part_1 <- 1:n_per_arm
  part_2 <- part_1 + total_subj / 2
  d[c(part_1, part_2), ]
}

summarize_results <- function(x) {
  x |>
    mutate(analysis_amount = as.numeric(analysis_amount)) |>
    group_by(.sim) |>
    # don't mutate over win ratio - just fill down if there is a 1 in anything
    mutate(across(-c(.sim, win_ratio), function(x) cumsum(x) > 0L)) |>
    group_by(analysis_amount) |>
    summarize(across(-.sim, mean))
}

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
