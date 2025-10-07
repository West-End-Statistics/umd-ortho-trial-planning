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
  
  int wins = 0;
  int losses = 0;
  int ties = 0;
  
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
  
  // No need to store individual comparisons for beta-binomial approach
  
  // Proportions
  real win_prop = wins / total_comparisons;
  real loss_prop = losses / total_comparisons;
  real tie_prop = ties / total_comparisons;
}

parameters {
  // Treatment effect only
  real log_wr;                        // Global hierarchical win ratio (death → amb → days)
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
  
  // Normal likelihood with proper U-statistic variance (more precise than simple effective n)
  if (wins + losses > 0) {
    real observed_win_prop = wins * 1.0 / (wins + losses);
    
    // Proper U-statistic variance formula accounting for correlation
    real harmonic_mean = 2.0 * n_control * n_treatment / (n_control + n_treatment);
    real variance_factor = 1.0 + (n_control - 1) / (2.0 * n_control) + (n_treatment - 1) / (2.0 * n_treatment);
    real ustat_var = expected_win_prob * (1 - expected_win_prob) * variance_factor / harmonic_mean;
    ustat_var += 1e-8;  // Numerical stability
    
    observed_win_prop ~ normal(expected_win_prob, sqrt(ustat_var));
  }
}

generated quantities {
  // Summary statistics (hierarchical)
  real total_wins = wins;
  real total_losses = losses;
  real total_ties = ties;
  
  // Net treatment benefit (delta)
  real delta = (wins - losses) * 1.0 / (n_control * n_treatment);
  
  // Probability of treatment benefit
  real prob_benefit = wr > 1 ? 1 : 0;
}
