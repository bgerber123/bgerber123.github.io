model {
  
  # Priors and linear models: shared for models of all three data sets
  alpha ~ dunif(-10, 10)                                 # Abundance intercept on log scale
  beta ~ dnorm(0, 1/10^2)                                # Slope on elevation
  
  # Joint likelihood: Note identical alpha and beta for all data sets
  # Likelihood portion for data set 1: regular counts
  for (i in 1:nsites1){
    C1[i] ~ dpois(lambda1[i])
    log(lambda1[i]) <- alpha + beta * selev1[i]
  }
  # Likelihood portion for data set 2: zero-truncated counts
  for (j in 1:nsites2){
    C2[j] ~ dpois(lambda2[j])T(1,)
    log(lambda2[j]) <- alpha + beta * selev2[j]
  }
  # Likelihood portion for data set 3: detection/nondetection
  for (k in 1:nsites3){
    y[k] ~ dbern(psi[k])
    cloglog(psi[k]) <- alpha + beta * selev3[k]
  }
}