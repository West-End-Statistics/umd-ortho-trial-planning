data {
  real win_ratio;
  real win_ratio_ste;

  real prior_mean;
  real prior_std;
}

parameters {
  real theta_raw;
}

model {
  theta_raw ~ normal(prior_mean, prior_std);
  win_ratio ~ normal(theta_raw, win_ratio_ste);
}

generated quantities {
   real theta;
   theta = exp(theta_raw);
}
