# Single-season, single-species hierarchical occupancy simulation

occ.rand.int.multico.simulate.fn <- function(N = 200,
                               J = 3,
                               alpha_psi = -0.5,
                               beta_psi1 = 1.0,
                               beta_psi2 = 0.8,
                               alpha_p = -1.0,
                               beta_p1 = -0.8,
                               beta_p2 = -0.6,
                               sigma_psi = 0.7,
                               sigma_p = 0.5,
                               rho_x = 0.9,
                               seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Two highly correlated site-level covariates from a multivariate normal
  mu <- c(0, 0)
  Sigma <- matrix(c(1, rho_x,
                    rho_x, 1), nrow = 2, byrow = TRUE)
  X <- MASS::mvrnorm(n = N, mu = mu, Sigma = Sigma)
  x1 <- X[, 1]
  x2 <- X[, 2]
  
  # Random intercepts
  u_psi <- rnorm(N, mean = 0, sd = sigma_psi)
  u_p <- rnorm(N, mean = 0, sd = sigma_p)
  
  # Occupancy model
  lt_psi <- alpha_psi + beta_psi1 * x1 + beta_psi2 * x2 + u_psi
  psi <- plogis(lt_psi)
  z <- rbinom(N, size = 1, prob = psi)
  
  # Detection model
  y <- matrix(0, nrow = N, ncol = J)
  p <- matrix(NA, nrow = N, ncol = J)
  
  for (i in seq_len(N)) {
    lt_p <- alpha_p + beta_p1 * x1[i] + beta_p2 * x2[i] + u_p[i]
    for (j in seq_len(J)) {
      p[i, j] <- plogis(lt_p)
      y[i, j] <- rbinom(1, size = 1, prob = z[i] * p[i, j])
    }
  }
  
  list(
    N = N,
    J = J,
    x1 = x1,
    x2 = x2,
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
# cor(sim$x1, sim$x2)
# head(sim$y)

# Optional: create a long-format data frame
long_format <- function(sim) {
  data.frame(
    site = rep(seq_len(sim$N), each = sim$J),
    occasion = rep(seq_len(sim$J), times = sim$N),
    x1 = rep(sim$x1, each = sim$J),
    x2 = rep(sim$x2, each = sim$J),
    z = rep(sim$z, each = sim$J),
    y = as.vector(t(sim$y))
  )
}

# Example:
# df <- long_format(sim)
# head(df)
