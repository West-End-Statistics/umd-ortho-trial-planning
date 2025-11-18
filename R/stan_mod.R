make_stan_data <- function(x, prior_mean = 0, prior_std = 1) {
  list(
    # transformations based on u - statistic
    win_ratio = log(x$estimate),
    win_ratio_ste = x$std.error / x$estimate,
    prior_mean = prior_mean,
    prior_std = prior_std
  )
}


create_cmdstan_model <- function(dir = tools::R_user_dir("umdorthotrialplanning")) {
  file <- system.file("stanmod", "meta.stan", package = "umdorthotrialplanning")

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = F)
  }

  cmdstanr::cmdstan_model(file, dir = dir)
}
