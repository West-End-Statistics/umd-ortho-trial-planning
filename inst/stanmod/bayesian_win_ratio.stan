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
  // Compute hierarchical comparisons properly
  // Each pair is decided at the first differentiating endpoint
  
  real wins = 0;
  real losses = 0;
  real ties = 0;
  
  // Marginal endpoint statistics (for comparison with BuyseTest)
  // These count ALL comparisons for each endpoint independently
  real death_wins_marginal = 0;
  real death_losses_marginal = 0;
  real death_ties_marginal = 0;
  
  real amb_wins_marginal = 0;
  real amb_losses_marginal = 0;
  real amb_ties_marginal = 0;
  
  real days_wins_marginal = 0;
  real days_losses_marginal = 0;
  real days_ties_marginal = 0;
  
  // Compute all pairwise comparisons
  for (i in 1:n_control) {
    for (j in 1:n_treatment) {
      // MARGINAL COMPARISONS (independent for each endpoint)
      // Death comparisons
      if (death_control[i] == 1 && death_treatment[j] == 0) {
        death_wins_marginal += 1;
      } else if (death_control[i] == 0 && death_treatment[j] == 1) {
        death_losses_marginal += 1;
      } else {
        death_ties_marginal += 1;
      }
      
      // Ambulatory comparisons (independent of death)
      real amb_diff = amb_treatment[j] - amb_control[i];
      if (amb_diff > amb_threshold) {
        amb_wins_marginal += 1;
      } else if (amb_diff < -amb_threshold) {
        amb_losses_marginal += 1;
      } else {
        amb_ties_marginal += 1;
      }
      
      // Days comparisons (independent of death and ambulatory)
      real days_diff = days_treatment[j] - days_control[i];
      if (days_diff > days_threshold) {
        days_wins_marginal += 1;
      } else if (days_diff < -days_threshold) {
        days_losses_marginal += 1;
      } else {
        days_ties_marginal += 1;
      }
      
      // HIERARCHICAL COMPARISON (for global win ratio)
      // First check death (binary, lower is better)
      if (death_control[i] == 1 && death_treatment[j] == 0) {
        // Control died, treatment didn't -> treatment wins
        wins += 1;
      } else if (death_control[i] == 0 && death_treatment[j] == 1) {
        // Treatment died, control didn't -> control wins (loss for treatment)
        losses += 1;
      } else {
        // Tie on death, check ambulatory status
        if (amb_diff > amb_threshold) {
          // Treatment has better ambulatory status
          wins += 1;
        } else if (amb_diff < -amb_threshold) {
          // Control has better ambulatory status
          losses += 1;
        } else {
          // Tie on ambulatory, check days at home
          if (days_diff > days_threshold) {
            // Treatment has more days at home
            wins += 1;
          } else if (days_diff < -days_threshold) {
            // Control has more days at home
            losses += 1;
          } else {
            // Complete tie
            ties += 1;
          }
        }
      }
    }
  }
  
  // Total comparisons
  real total_comparisons = n_control * n_treatment;
  
  // Proportions
  real win_prop = wins / total_comparisons;
  real loss_prop = losses / total_comparisons;
  real tie_prop = ties / total_comparisons;
}

parameters {
  // Log win ratio parameters
  real log_wr;           // Global hierarchical win ratio
  real log_wr_death;     // Death endpoint win ratio
  real log_wr_amb;       // Ambulatory endpoint win ratio  
  real log_wr_days;      // Days endpoint win ratio
}

transformed parameters {
  // Win ratios (always positive)
  real wr = exp(log_wr);
  real wr_death = exp(log_wr_death);
  real wr_amb = exp(log_wr_amb);
  real wr_days = exp(log_wr_days);
  
  // Expected win probabilities using inv_logit for stability
  real expected_win_prob = inv_logit(log_wr);
  real expected_win_prob_death = inv_logit(log_wr_death);
  real expected_win_prob_amb = inv_logit(log_wr_amb);
  real expected_win_prob_days = inv_logit(log_wr_days);
}

model {
  // Priors
  log_wr ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  log_wr_death ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  log_wr_amb ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  log_wr_days ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  
  // Likelihood for the hierarchical win ratio
  if (wins + losses > 0) {
    real observed_win_prop = wins / (wins + losses);
    real n_eff = 2.0 * n_control * n_treatment / (n_control + n_treatment);
    real var_ustat = expected_win_prob * (1 - expected_win_prob) / n_eff;
    var_ustat += 1e-6;
    observed_win_prop ~ normal(expected_win_prob, sqrt(var_ustat));
  }
  
  // Likelihoods for individual endpoints (marginal)
  if (death_wins_marginal + death_losses_marginal > 0) {
    real death_obs_prop = death_wins_marginal / (death_wins_marginal + death_losses_marginal);
    real n_eff = 2.0 * n_control * n_treatment / (n_control + n_treatment);
    real var_ustat = expected_win_prob_death * (1 - expected_win_prob_death) / n_eff;
    var_ustat += 1e-6;
    death_obs_prop ~ normal(expected_win_prob_death, sqrt(var_ustat));
  }
  
  if (amb_wins_marginal + amb_losses_marginal > 0) {
    real amb_obs_prop = amb_wins_marginal / (amb_wins_marginal + amb_losses_marginal);
    real n_eff = 2.0 * n_control * n_treatment / (n_control + n_treatment);
    real var_ustat = expected_win_prob_amb * (1 - expected_win_prob_amb) / n_eff;
    var_ustat += 1e-6;
    amb_obs_prop ~ normal(expected_win_prob_amb, sqrt(var_ustat));
  }
  
  if (days_wins_marginal + days_losses_marginal > 0) {
    real days_obs_prop = days_wins_marginal / (days_wins_marginal + days_losses_marginal);
    real n_eff = 2.0 * n_control * n_treatment / (n_control + n_treatment);
    real var_ustat = expected_win_prob_days * (1 - expected_win_prob_days) / n_eff;
    var_ustat += 1e-6;
    days_obs_prop ~ normal(expected_win_prob_days, sqrt(var_ustat));
  }
}

generated quantities {
  // Summary statistics (hierarchical)
  real total_wins = wins;
  real total_losses = losses;
  real total_ties = ties;
  
  // Marginal summary statistics (for comparison with BuyseTest)
  real death_favorable = death_wins_marginal;
  real death_unfavorable = death_losses_marginal;
  real death_neutral = death_ties_marginal;
  
  real amb_favorable = amb_wins_marginal;
  real amb_unfavorable = amb_losses_marginal;
  real amb_neutral = amb_ties_marginal;
  
  real days_favorable = days_wins_marginal;
  real days_unfavorable = days_losses_marginal;
  real days_neutral = days_ties_marginal;
  
  // Net treatment benefit (delta) - hierarchical
  real delta = (wins - losses) / (n_control * n_treatment);
  
  // Probability of treatment benefit
  real prob_benefit = wr > 1 ? 1 : 0;
  real prob_benefit_death = wr_death > 1 ? 1 : 0;
  real prob_benefit_amb = wr_amb > 1 ? 1 : 0;
  real prob_benefit_days = wr_days > 1 ? 1 : 0;
}
