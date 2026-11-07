sim.data.logistic.mixed2.model = function(n_groups,
                                         n_per_group,
                                         beta_0,
                                         beta_1,
                                         sd_int){

  # n_groups <- 50 ; n_per_group <- 20  ; beta_0 <- -1  ; beta_1 <- 2 ; sd_int <- 0.75   
  
  # Random intercept and fixed slopes    

  # n_groups <- 50          # number of groups
  # n_per_group <- 20       # observations per group
  
  # beta_0 <- -1            # fixed (mean) intercept
  # beta_1 <- 2             # fixed slope
  # 
  # sd_int <- 0.75        # standard deviation of random intercepts
  
  # -----------------------------
  # Create group-level random slopes
  # -----------------------------
  group_id <- rep(1:n_groups, each = n_per_group)
  
  random_int <- rnorm(n_groups, mean = 0, sd = sd_int)

  # -----------------------------
  # Predictor variable
  # -----------------------------
  x <- rnorm(n_groups * n_per_group)
  
  # -----------------------------
  # Linear predictor and probability
  # -----------------------------
  lt <- (beta_0 + random_int[group_id]) + beta_1 * x
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
    parms = list(beta_0, beta_1,sd_int)
  )

  return(sim_data)
  
}
