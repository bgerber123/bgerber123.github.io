model {
  
  # Priors
  beta0 ~ dunif(-10, 10)   
  beta1 ~ dnorm(0, 1/10^2)  
  
  # Likelihood portion for data set 2: zero-truncated counts
  for (j in 1:nsites2){
    C2[j] ~ dpois(lambda2[j])T(1,)
    log(lambda2[j]) <- beta0 + beta1 * selev2[j]
  }
}