data {
  real win_ratio;
  real win_ratio_ste;

  real prior_mean;
  real prior_std;
}

parameters {
  real theta;
}

model {
  theta ~ normal(prior_mean, prior_std);
  win_ratio ~ normal(theta, win_ratio_ste);
}
