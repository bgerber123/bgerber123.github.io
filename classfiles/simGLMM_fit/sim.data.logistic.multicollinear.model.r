sim.data.logistic.multicollinear.model = function(n,
                                   beta_0,
                                   beta_1,
                                   beta_2,
                                   rho_x =0.9){

   # n <- 50 ; beta_0 <- -1  ; beta_1 <- 2  ; beta_2 <- -2; rho_x = 0.9
  
    


  # -----------------------------
  # Predictor variable
  # -----------------------------
  mu <- c(0, 0)
  
  Sigma <- matrix(c(1, rho_x,
                    rho_x, 1), nrow = 2, byrow = TRUE)
  X <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
  x1 <- X[, 1]
  x2 <- X[, 2]
  
  cor(x1,x2)
  
  # -----------------------------
  # Linear predictor and probability
  # -----------------------------
  lt <- beta_0 + beta_1 * x1 + beta_2 * x2
  p <- plogis(lt)
  
  # -----------------------------
  # Simulate binary outcome
  # -----------------------------
  y <- rbinom(n, size = 1, prob = p)
  
  # -----------------------------
  # Assemble data frame
  # -----------------------------
  sim_data <- list(
    x = data.frame(x1 = x1,x2=x2),
    y = y,
    parms = list(beta_0, beta_1,beta_2,rho_x)
  )

  return(sim_data)
  
}
