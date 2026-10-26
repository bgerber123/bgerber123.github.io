# Example usage
# sim <- occ.simulate.fn(N = 150, J = 4, seed = 123)
# str(sim)
# head(sim$y)


occ.simulate.fn <- function(N = 200, #number of sites
                               J = 5,   #number of occassions
                               alpha_psi = -0.5, # occ intercept
                               beta_psi = 1.0,   # occ slope
                               alpha_p = -1.0,   # dec intercept
                               beta_p = 1.0,     # dec slope
                               seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Site-level covariate for occupancy (only varies by site)
  X <- rnorm(N, mean = 0, sd = 1)
  
  # Occupancy model
  lt_psi <- alpha_psi + beta_psi * X
  psi <- plogis(lt_psi)
  z <- rbinom(N, size = 1, prob = psi)
  
  # Detection covariate - that varies by site and occasion (weather, maybe)
  W <- matrix(rnorm(N * J, mean = 0, sd = 1), nrow = N, ncol = J)
  
  # Detection model
  lt_p <- alpha_p + beta_p * W
  p <- plogis(lt_p)
  
  # Detection histories
  y <- matrix(0, nrow = N, ncol = J)
  for (i in seq_len(N)) {
    for (j in seq_len(J)) {
      y[i, j] <- rbinom(1, size = 1, prob = z[i] * p[i, j])
    }
  }
  
  list(
    N = N,
    J = J,
    X = data.frame(site.cov=X),
    W = W,
    z = z,
    psi = psi,
    y = y
  )
}



