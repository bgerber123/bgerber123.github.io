# Single-season, single-species hierarchical occupancy simulation

occ.rand.int.simulate.fn <- function(N = 200,
                               J = 3,
                               alpha_psi = -0.5,
                               beta_psi = 1.0,
                               alpha_p = -1.0,
                               beta_p = -0.8,
                               sigma_psi = 0.7,
                               sigma_p = 0.5,
                               seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Site-level covariate
  x <- rnorm(N, mean = 0, sd = 1)
  
  # Random intercept for occupancy
  u_psi <- rnorm(N, mean = 0, sd = sigma_psi)
  
  # Random intercept for detection
  u_p <- rnorm(N, mean = 0, sd = sigma_p)
  
  # Occupancy model: positive effect of x
  lt_psi <- alpha_psi + beta_psi * x + u_psi
  psi <- plogis(lt_psi)
  z <- rbinom(N, size = 1, prob = psi)
  
  # Detection model: negative effect of the same site covariate x
  y <- matrix(0, nrow = N, ncol = J)
  p <- matrix(NA, nrow = N, ncol = J)
  
  for (i in seq_len(N)) {
    for (j in seq_len(J)) {
      lt_p <- alpha_p + beta_p * x[i] + u_p[i]
      p[i, j] <- plogis(lt_p)
      y[i, j] <- rbinom(1, size = 1, prob = z[i] * p[i, j])
    }
  }
  
  list(
    N = N,
    J = J,
    X = X,
    u_psi = u_psi,
    u_p = u_p,
    z = z,
    psi = psi,
    p = p,
    y = y
  )
}

# Example usage
# sim <- simulate_occupancy(N = 150, J = 4, seed = 123)
# str(sim)
# head(sim$y)


