sim.data.logistic.mixed.model = function(n_groups,
                                         n_per_group,
                                         beta_0,
                                         beta_1,
                                         sd_slope){
  

  #n_groups <- 50          # number of groups
  #n_per_group <- 20       # observations per group
  
  # beta_0 <- -1            # fixed intercept
  # beta_1 <- 2             # fixed average slope
  # 
  # sd_slope <- 0.75        # standard deviation of random slope
  
  # -----------------------------
  # Create group-level random slopes
  # -----------------------------
  group_id <- rep(1:n_groups, each = n_per_group)
  
  random_slope <- rnorm(n_groups, mean = 0, sd = sd_slope)
  slope_i <- beta_1 + random_slope[group_id]
  
  # -----------------------------
  # Predictor variable
  # -----------------------------
  x <- rnorm(n_groups * n_per_group)
  
  # -----------------------------
  # Linear predictor and probability
  # -----------------------------
  lt <- beta_0 + slope_i * x
  p <- plogis(lt)
  
  # -----------------------------
  # Simulate binary outcome
  # -----------------------------
  y <- rbinom(n_groups * n_per_group, size = 1, prob = p)
  
  # -----------------------------
  # Assemble data frame
  # -----------------------------
  sim_data <- data.frame(
    group = factor(group_id),
    x = x,
    y = y,
    parms = list(beta_0, beta_1,sd_slope)
  )

  return(sim_data)
  
}
