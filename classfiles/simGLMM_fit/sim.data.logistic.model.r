sim.data.logistic.model = function(n,
                                   beta_0,
                                   beta_1){

   # n <- 50 ; beta_0 <- -1  ; beta_1 <- 2 
  
    


  # -----------------------------
  # Predictor variable
  # -----------------------------
  x <- rnorm(n)
  
  # -----------------------------
  # Linear predictor and probability
  # -----------------------------
  lt <- beta_0 + beta_1 * x
  p <- plogis(lt)
  
  # -----------------------------
  # Simulate binary outcome
  # -----------------------------
  y <- rbinom(n, size = 1, prob = p)
  
  # -----------------------------
  # Assemble data frame
  # -----------------------------
  sim_data <- list(
    x = x,
    y = y,
    parms = list(beta_0, beta_1)
  )

  return(sim_data)
  
}
