#' Simulate Full Clinical Trial with Both Arms
#'
#' Creates a complete trial dataset with both placebo and active treatment arms.
#' All parameters are passed to \code{\link{simulate_data}} for each arm.
#'
#' @param n_per_arm Integer. Number of subjects per arm.
#' @param ... Additional arguments passed to \code{\link{simulate_data}}.
#'
#' @return A data frame containing combined data from both arms with an additional
#'   'arm' column as a factor with levels c("placebo", "active").
#'
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' # Simulate trial with 100 patients per arm
#' trial_data <- simulate_trial(n_per_arm = 100)
#'
#' # Simulate trial with custom parameters
#' trial_data <- simulate_trial(
#'   n_per_arm = 200,
#'   outcome_correlation = 0.3,
#'   survival_prob = c(0.85, 0.90)
#' )
simulate_trial <- function(n_per_arm, ...) {
  out <- dplyr::bind_rows(
    placebo = simulate_data(
      n = n_per_arm,
      arm = "placebo",
      ...
    ),
    active = simulate_data(
      n = n_per_arm,
      arm = "active",
      ...
    ),
    .id = "arm"
  )

  out$arm <- factor(out$arm, c("placebo", "active"))
  out
}
