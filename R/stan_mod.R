#' Prepare Data for Stan Bayesian Model
#'
#' Transforms win ratio estimates into log-scale for Stan model input, along
#' with prior parameters.
#'
#' @param x Data frame. Win ratio estimates with columns: estimate, std.error.
#' @param prior_mean Numeric. Mean of prior distribution (default: 0).
#' @param prior_std Numeric. Standard deviation of prior distribution (default: 1).
#'
#' @return Named list with elements:
#'   \item{win_ratio}{Log-transformed win ratio estimate}
#'   \item{win_ratio_ste}{Standard error on log scale}
#'   \item{prior_mean}{Prior mean}
#'   \item{prior_std}{Prior standard deviation}
#'
#' @keywords internal
make_stan_data <- function(x, prior_mean = 0, prior_std = 1) {
  list(
    # transformations based on u - statistic
    win_ratio = log(x$estimate),
    win_ratio_ste = x$std.error / x$estimate,
    prior_mean = prior_mean,
    prior_std = prior_std
  )
}


#' Create and Compile CmdStan Model for Bayesian Analysis
#'
#' Compiles the Stan meta-analysis model for win ratio estimation. The compiled
#' model is cached in the user's R directory for reuse.
#'
#' @param dir Character. Directory for compiled Stan model (default: user R directory).
#'
#' @return CmdStanModel object. Compiled Stan model ready for sampling.
#'
#' @export
#' @importFrom cmdstanr cmdstan_model
create_cmdstan_model <- function(dir = tools::R_user_dir("umdorthotrialplanning")) {
  file <- system.file("stanmod", "meta.stan", package = "umdorthotrialplanning")

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  cmdstanr::cmdstan_model(file, dir = dir)
}
