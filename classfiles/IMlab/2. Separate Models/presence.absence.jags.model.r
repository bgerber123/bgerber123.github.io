model {
  
  # Priors
  beta0 ~ dunif(-10, 10)   
  beta1 ~ dnorm(0, 1/10^2)  
  
  # Likelihood portion for data set 3: presence/absence
  for (k in 1:nsites3){
    y[k] ~ dbern(psi[k])
    cloglog(psi[k]) <- beta0 + beta1 * selev3[k]
  }
  
}