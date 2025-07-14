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
  // Use the exact same logic as BuyseTest hierarchical comparisons
  // But calculate the final win ratio from the final hierarchical outcome
  
  real wins = 0;
  real losses = 0;
  real ties = 0;
  
  // Compute all pairwise comparisons hierarchically
  for (i in 1:n_control) {
    for (j in 1:n_treatment) {
      real amb_diff = amb_treatment[j] - amb_control[i];
      real days_diff = days_treatment[j] - days_control[i];
      
      // Hierarchical comparison: death → ambulatory → days
      if (death_control[i] == 1 && death_treatment[j] == 0) {
        // Control died, treatment didn't → treatment wins
        wins += 1;
      } else if (death_control[i] == 0 && death_treatment[j] == 1) {
        // Treatment died, control didn't → control wins
        losses += 1;
      } else {
        // Tie on death, check ambulatory
        if (amb_diff >= amb_threshold) {
          // Treatment has better ambulatory status
          wins += 1;
        } else if (amb_diff <= -amb_threshold) {
          // Control has better ambulatory status
          losses += 1;
        } else {
          // Tie on ambulatory, check days
          if (days_diff >= days_threshold) {
            // Treatment has more days at home
            wins += 1;
          } else if (days_diff <= -days_threshold) {
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
  real log_wr;           // Global hierarchical win ratio (death → amb → days)
}

transformed parameters {
  // Win ratios (always positive)
  real wr = exp(log_wr);
  
  // Expected win probabilities using inv_logit for stability
  real expected_win_prob = inv_logit(log_wr);
}

model {
  // Priors
  log_wr ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  
  // Likelihood for the hierarchical win ratio
  if (wins + losses > 0) {
    real observed_win_prop = wins / (wins + losses);
    real n_eff = 2.0 * n_control * n_treatment / (n_control + n_treatment);
    real var_ustat = expected_win_prob * (1 - expected_win_prob) / n_eff;
    var_ustat += 1e-6;
    observed_win_prop ~ normal(expected_win_prob, sqrt(var_ustat));
  }
}

generated quantities {
  // Summary statistics (hierarchical)
  real total_wins = wins;
  real total_losses = losses;
  real total_ties = ties;
  
  // Net treatment benefit (delta)
  real delta = (wins - losses) / (n_control * n_treatment);
  
  // Probability of treatment benefit
  real prob_benefit = wr > 1 ? 1 : 0;
}
