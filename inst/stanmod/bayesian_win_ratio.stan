data {
  // Sample sizes
  int<lower=1> n_control;
  int<lower=1> n_treatment;
  
  // Endpoint 1: Death (binary, lower is better)
  vector<lower=0, upper=1>[n_control] death_control;
  vector<lower=0, upper=1>[n_treatment] death_treatment;
  
  // Endpoint 2: Ambulatory status (continuous, higher is better)
  vector[n_control] amb_control;
  vector[n_treatment] amb_treatment;
  real<lower=0> amb_threshold;
  
  // Endpoint 3: Days at home (continuous, higher is better)
  vector[n_control] days_control;
  vector[n_treatment] days_treatment;
  real<lower=0> days_threshold;
  
  // Priors
  real prior_mean_log_wr;
  real<lower=0> prior_sd_log_wr;
}

transformed data {
  int n_pairs = n_control * n_treatment;
  
  // Pre-compute all pairwise comparisons for each endpoint
  matrix[n_control, n_treatment] death_wins;
  matrix[n_control, n_treatment] death_losses;
  matrix[n_control, n_treatment] amb_wins;
  matrix[n_control, n_treatment] amb_losses;
  matrix[n_control, n_treatment] days_wins;
  matrix[n_control, n_treatment] days_losses;
  
  // Death comparisons (treatment wins if control dies and treatment doesn't)
  for (i in 1:n_control) {
    for (j in 1:n_treatment) {
      death_wins[i,j] = (death_control[i] == 1 && death_treatment[j] == 0) ? 1 : 0;
      death_losses[i,j] = (death_control[i] == 0 && death_treatment[j] == 1) ? 1 : 0;
    }
  }
  
  // Ambulatory status comparisons (treatment wins if difference > threshold)
  for (i in 1:n_control) {
    for (j in 1:n_treatment) {
      real diff = amb_treatment[j] - amb_control[i];
      amb_wins[i,j] = (diff > amb_threshold) ? 1 : 0;
      amb_losses[i,j] = (diff < -amb_threshold) ? 1 : 0;
    }
  }
  
  // Days at home comparisons (treatment wins if difference > threshold)
  for (i in 1:n_control) {
    for (j in 1:n_treatment) {
      real diff = days_treatment[j] - days_control[i];
      days_wins[i,j] = (diff > days_threshold) ? 1 : 0;
      days_losses[i,j] = (diff < -days_threshold) ? 1 : 0;
    }
  }
  
  // Compute hierarchical win/loss counts
  int global_wins = 0;
  int global_losses = 0;
  
  // Individual endpoint win/loss counts
  int death_wins_count = 0;
  int death_losses_count = 0;
  int amb_wins_count = 0;
  int amb_losses_count = 0;
  int days_wins_count = 0;
  int days_losses_count = 0;
  
  for (i in 1:n_control) {
    for (j in 1:n_treatment) {
      // Count individual endpoint comparisons
      if (death_wins[i,j] == 1) death_wins_count += 1;
      if (death_losses[i,j] == 1) death_losses_count += 1;
      if (amb_wins[i,j] == 1) amb_wins_count += 1;
      if (amb_losses[i,j] == 1) amb_losses_count += 1;
      if (days_wins[i,j] == 1) days_wins_count += 1;
      if (days_losses[i,j] == 1) days_losses_count += 1;
      
      // Hierarchical decision for this pair
      if (death_wins[i,j] == 1) {
        global_wins += 1;
      } else if (death_losses[i,j] == 1) {
        global_losses += 1;
      } else if (amb_wins[i,j] == 1) {
        global_wins += 1;
      } else if (amb_losses[i,j] == 1) {
        global_losses += 1;
      } else if (days_wins[i,j] == 1) {
        global_wins += 1;
      } else if (days_losses[i,j] == 1) {
        global_losses += 1;
      }
    }
  }
}

parameters {
  // Log win ratios for each endpoint (unconstrained)
  real log_wr_death;
  real log_wr_amb;
  real log_wr_days;
  real log_wr_global;
}

transformed parameters {
  // Win ratios (always positive)
  real wr_death = exp(log_wr_death);
  real wr_amb = exp(log_wr_amb);
  real wr_days = exp(log_wr_days);
  real wr_global = exp(log_wr_global);
  
  // Probabilities for binomial likelihood using inv_logit for numerical stability
  real<lower=0, upper=1> p_death = inv_logit(log_wr_death);
  real<lower=0, upper=1> p_amb = inv_logit(log_wr_amb);
  real<lower=0, upper=1> p_days = inv_logit(log_wr_days);
  real<lower=0, upper=1> p_global = inv_logit(log_wr_global);
}

model {
  // Priors on log scale for numerical stability
  log_wr_death ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  log_wr_amb ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  log_wr_days ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  log_wr_global ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  
  // Likelihood using sufficient statistics approach
  // Only include likelihoods when there are actual comparisons
  
  // Global hierarchical likelihood
  if (global_wins + global_losses > 0) {
    global_wins ~ binomial(global_wins + global_losses, p_global);
  }
  
  // Individual endpoint likelihoods
  if (death_wins_count + death_losses_count > 0) {
    death_wins_count ~ binomial(death_wins_count + death_losses_count, p_death);
  }
  
  if (amb_wins_count + amb_losses_count > 0) {
    amb_wins_count ~ binomial(amb_wins_count + amb_losses_count, p_amb);
  }
  
  if (days_wins_count + days_losses_count > 0) {
    days_wins_count ~ binomial(days_wins_count + days_losses_count, p_days);
  }
}

generated quantities {
  // Calculate summary statistics
  real death_favorable = death_wins_count;
  real death_unfavorable = death_losses_count;
  real death_neutral = n_pairs - death_favorable - death_unfavorable;
  
  real amb_favorable = amb_wins_count;
  real amb_unfavorable = amb_losses_count;
  real amb_neutral = n_pairs - amb_favorable - amb_unfavorable;
  
  real days_favorable = days_wins_count;
  real days_unfavorable = days_losses_count;
  real days_neutral = n_pairs - days_favorable - days_unfavorable;
  
  // Net treatment benefit (delta)
  real delta_death = (death_favorable - death_unfavorable) * 1.0 / n_pairs;
  real delta_amb = (amb_favorable - amb_unfavorable) * 1.0 / n_pairs;
  real delta_days = (days_favorable - days_unfavorable) * 1.0 / n_pairs;
  real delta_global = (global_wins - global_losses) * 1.0 / n_pairs;
  
  // Probability of treatment benefit
  real prob_benefit_death = wr_death > 1 ? 1 : 0;
  real prob_benefit_amb = wr_amb > 1 ? 1 : 0;
  real prob_benefit_days = wr_days > 1 ? 1 : 0;
  real prob_benefit_global = wr_global > 1 ? 1 : 0;
}
