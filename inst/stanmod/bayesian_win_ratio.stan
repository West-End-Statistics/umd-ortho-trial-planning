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
  
  // Individual endpoint statistics (for comparison with BuyseTest)
  real death_wins = 0;
  real death_losses = 0;
  real death_ties = 0;
  
  real amb_wins = 0;
  real amb_losses = 0;
  real amb_ties = 0;
  
  real days_wins = 0;
  real days_losses = 0;
  real days_ties = 0;
  
  // Compute all pairwise comparisons hierarchically
  for (i in 1:n_control) {
    for (j in 1:n_treatment) {
      // First check death (binary, lower is better)
      if (death_control[i] == 1 && death_treatment[j] == 0) {
        // Control died, treatment didn't -> treatment wins
        wins += 1;
        death_wins += 1;
      } else if (death_control[i] == 0 && death_treatment[j] == 1) {
        // Treatment died, control didn't -> control wins (loss for treatment)
        losses += 1;
        death_losses += 1;
      } else {
        // Tie on death, check ambulatory status
        death_ties += 1;
        
        real amb_diff = amb_treatment[j] - amb_control[i];
        if (amb_diff > amb_threshold) {
          // Treatment has better ambulatory status
          wins += 1;
          amb_wins += 1;
        } else if (amb_diff < -amb_threshold) {
          // Control has better ambulatory status
          losses += 1;
          amb_losses += 1;
        } else {
          // Tie on ambulatory, check days at home
          amb_ties += 1;
          
          real days_diff = days_treatment[j] - days_control[i];
          if (days_diff > days_threshold) {
            // Treatment has more days at home
            wins += 1;
            days_wins += 1;
          } else if (days_diff < -days_threshold) {
            // Control has more days at home
            losses += 1;
            days_losses += 1;
          } else {
            // Complete tie
            ties += 1;
            days_ties += 1;
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
  // Log win ratio parameter
  real log_wr;
}

transformed parameters {
  // Win ratio (always positive)
  real wr = exp(log_wr);
  
  // Expected win probability under the model using inv_logit for stability
  real expected_win_prob = inv_logit(log_wr);
}

model {
  // Prior
  log_wr ~ normal(prior_mean_log_wr, prior_sd_log_wr);
  
  // Likelihood for the hierarchical win ratio
  // Use proper U-statistic variance approximation
  if (wins + losses > 0) {
    // Observed win proportion among decided pairs
    real observed_win_prop = wins / (wins + losses);
    
    // Variance approximation for U-statistic
    // Use harmonic mean of sample sizes for variance calculation
    real n_eff = 2.0 * n_control * n_treatment / (n_control + n_treatment);
    real var_ustat = expected_win_prob * (1 - expected_win_prob) / n_eff;
    
    // Add small constant to prevent numerical issues
    var_ustat += 1e-6;
    
    observed_win_prop ~ normal(expected_win_prob, sqrt(var_ustat));
  }
}

generated quantities {
  // Individual endpoint win ratios (for comparison with BuyseTest)
  real death_wr = death_wins > 0 && death_losses > 0 ? 
    death_wins / death_losses : 1.0;
  
  real amb_wr = amb_wins > 0 && amb_losses > 0 ? 
    amb_wins / amb_losses : 1.0;
    
  real days_wr = days_wins > 0 && days_losses > 0 ? 
    days_wins / days_losses : 1.0;
  
  // Summary statistics
  real total_wins = wins;
  real total_losses = losses;
  real total_ties = ties;
  
  real death_favorable = death_wins;
  real death_unfavorable = death_losses;
  real death_neutral = death_ties;
  
  real amb_favorable = amb_wins;
  real amb_unfavorable = amb_losses;
  real amb_neutral = amb_ties;
  
  real days_favorable = days_wins;
  real days_unfavorable = days_losses;
  real days_neutral = days_ties;
  
  // Net treatment benefit (delta)
  real delta = (wins - losses) / (n_control * n_treatment);
  
  // Probability of treatment benefit
  real prob_benefit = wr > 1 ? 1 : 0;
}