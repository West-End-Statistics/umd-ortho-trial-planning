library(dplyr)
library(WinRatio)
library(ordinal)
library(truncnorm)
library(ggplot2)
library(tidyr)


## Simulate trial data
n=200 # per arm
weibull_shape=9
weibull_scale = 180

dt = tibble(treat = rep(0:1,each=n)) %>% 
  mutate(death = ifelse(treat==1,rbinom(n,1,0.10),rbinom(n,1,.15)),
         id = row_number(),
         maxfu = 120,
         death.time=rweibull(n*2, shape = weibull_shape, scale = weibull_scale),
         death.time=ifelse(death==1,rtruncnorm(sum(death==1),a=1,b=90,45,10),death.time),
         death.time=pmin(death.time,maxfu),
         amb = ifelse(treat==1,sample(c(0,1,2,3),n, replace=T, prob = c(0.10,0.30,0.40,0.20)),
                      sample(c(0,1,2,3),n, replace=T, prob = c(0.15,0.35,0.35,0.15))),
         daysathome = ifelse(treat==1,rtruncnorm(n,a=0,b=120,74,20),rtruncnorm(n,a=0,b=100,63,20)),
         daysathome = round(daysathome,0))

# Estimate win ratio
wr1=winratio(id="id",trt="treat",active=1, 
             outcomes = list(outc1 = c("death","s","death.time"),
                             outc2 = c("amb","c","<"),
                             outc3 = c("daysathome","c","<")),
             fu = "maxfu", data = dt, keep.matrix = T)
summary(wr1)
log(wr1$wr) # point estimate
log(wr1$wr)/wr1$z # standard error from win ratio model


## Extract pairwise comparison and structure based on of win results for treatment and control group
dt.mat = data.frame(wr1$wr.matrix) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = c(X1:X200),
               names_to='pair',
               values_to = 'outcome') %>% 
  mutate(treat = ifelse(outcome>0,1,0),
         result = case_when(
           abs(outcome)==3 ~ 1,
           abs(outcome)==2 ~ 2,
           abs(outcome)==1 ~ 3,
           TRUE ~ NA         )) %>% 
  mutate(result = factor(result, ordered = T),
         weight = 400/(200*200)) ## I'm not sure what the formula should be here

## Calculate proportion odds from the win ratio matrix
f1 <- clm(result ~ treat ,weights = weight , 
             data = dt.mat)
summary(f1)
exp(0.2862)


#### 


# Approach of combining the prior with win ratio likelihood function
# Function to compute posterior distribution with probability calculations
compute_posterior <- function(prior_distribution, likelihood, parameters) {
  # Prior distribution
  prior <- do.call(paste0("d", prior_distribution), 
                   c(list(x = parameters$x), 
                     prior_distribution_params(prior_distribution, parameters)))
  
  # Likelihood estimation
  likelihood_func <- do.call(paste0("d", likelihood), 
                             c(list(x = parameters$x), 
                               likelihood_distribution_params(likelihood, parameters)))
  
  # Compute posterior (unnormalized)
  posterior <- prior * likelihood_func
  
  # Normalize posterior
  posterior_normalized <- posterior / sum(posterior)
  
  # Calculate posterior mean
  posterior_mean <- sum(parameters$x * posterior_normalized)
  
  # Calculate 95% credible interval
  cumulative_posterior <- cumsum(posterior_normalized)
  lower_index <- which.min(abs(cumulative_posterior - 0.025))
  upper_index <- which.min(abs(cumulative_posterior - 0.975))
  
  credible_interval <- c(parameters$x[lower_index], parameters$x[upper_index])
  
  # Calculate probabilities
  prob_greater_0 <- sum(posterior_normalized[parameters$x > 0])
  prob_greater_0.262 <- sum(posterior_normalized[parameters$x > 0.262])
  
  return(list(
    prior = prior / sum(prior),
    likelihood = likelihood_func / sum(likelihood_func),
    posterior = posterior_normalized,
    x_values = parameters$x,
    posterior_mean = posterior_mean,
    credible_interval = credible_interval,
    prob_greater_0 = prob_greater_0,
    prob_greater_0.262 = prob_greater_0.262
  ))
}

# Updated parameters
example_parameters <- list(
  x = seq(-3, 3, by = 0.1),  # Range of values
  prior_distribution = "norm",
  likelihood = "norm",
  prior_mean = 0,
  prior_sd = 1,
  likelihood_mean = log(wr1$wr),
  likelihood_se = (log(wr1$wr))/wr1$z
)

# Compute posterior
result <- compute_posterior(
  prior_distribution = example_parameters$prior_distribution,
  likelihood = example_parameters$likelihood,
  parameters = example_parameters
)

# Exponentiate results
exp_posterior_mean <- exp(result$posterior_mean)
exp_credible_interval <- exp(result$credible_interval)

# Print results
cat("Posterior Mean (exponentiated):", exp_posterior_mean, "\n")
cat("95% Credible Interval (exponentiated):", 
    exp_credible_interval[1], "to", exp_credible_interval[2], "\n")
cat("Probability posterior mean > WR=1:", result$prob_greater_0, "\n")
cat("Probability posterior mean > WR=1.3:", result$prob_greater_0.262, "\n")

# Prepare data for plotting
plot_data <- data.frame(
  x = example_parameters$x,
  prior = result$prior,
  likelihood = result$likelihood,
  posterior = result$posterior
) %>% 
  pivot_longer(cols = c(prior, likelihood, posterior), 
               names_to = "distribution", 
               values_to = "density")

# Plot using ggplot2
ggplot(plot_data, aes(x = x, y = density, color = distribution)) +
  geom_line(size=1.5, alpha=0.5) +
  theme_minimal() +
  labs(
    title = "Prior, Likelihood, and Posterior Distributions",
    x = "Parameter Value",
    y = "Density"
  ) +
  scale_color_manual(values = c("prior" = "blue", "likelihood" = "red", "posterior" = "green"))
