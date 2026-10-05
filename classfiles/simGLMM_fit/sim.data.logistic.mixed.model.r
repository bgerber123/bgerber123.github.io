sim.data.logistic.mixed.model = function(n_groups,n_per_group,
                                         beta_0,
                                         beta_1,
                                         beta_2,
                                         sigma_u){
  

# --- 1. Simulation Setup ---
  n_groups <- 20       # Number of groups (e.g., sites, subjects)
  n_per_group <- 25    # Number of observations per group
  N <- sum(n_groups * n_per_group)  
  
# Fixed effects (intercept and slopes)
  # beta_0 <- -0.5       # Baseline log-odds
  # beta_1 <-  0.8       # Effect of continuous covariate X1
  # beta_2 <- -0.4       # Effect of continuous covariate X2
  # 
# Random effects variance
#  sigma_u <- 1.2       # Standard deviation of random intercepts
  
# --- 2. Generate Data Structure ---
# Group identifiers
  group_id <- factor(rep(1:n_groups, each = n_per_group))
  
# Continuous covariates
  # X1: standard normal covariate
  x1 <- rnorm(N, mean = 0, sd = 1)
  
  # X2: continuous covariate with a mean of 5 and sd of 2
  x2 <- rnorm(N, mean = 5, sd = 2)
  
# --- 3. Simulate Random Effects & Linear Predictor ---
  # Generate one random intercept per group: u_j ~ N(0, sigma_u^2)
  u_j <- rnorm(n_groups, mean = 0, sd = sigma_u)
  
# Assign group-level random intercepts to individual observations
  u_i <- u_j[group_id]

# Linear predictor (eta)
  lt <- beta_0 + beta_1 * x1 + beta_2 * x2 + u_i
  
  # --- 4. Apply Inverse-Link & Simulate Outcome ---
  # Inverse logit function to get probabilities
  p <- 1 / (1 + exp(-lt)) # equivalent to plogis(eta)
  
# Simulate binary outcome Y ~ Bernoulli(p)
  y <- rbinom(N, size = 1, prob = p)
  
# Combine into a data frame
  sim_data <- data.frame(
    group = group_id,
    x1 = x1,
    x2 = x2,
    p = p,
    y = y
  )
  
  sim_data
  
}