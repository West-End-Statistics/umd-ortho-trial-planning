# The following function creates a full trial with both arms
simulate_trial <- function(n_per_arm, ...) {
  out <- dplyr::bind_rows(
    placebo = simulate_data(
      n = n_per_arm,
      arm = "placebo"
    ),
    active = simulate_data(
      n = n_per_arm,
      arm = "active"
    ),
    .id = "arm"
  )

  out$arm <- factor(out$arm, c("placebo", "active"))
  out
}
