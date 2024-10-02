model {
  
  # Priors
  alpha ~ dunif(-10, 10)   
  beta ~ dnorm(0, 1/10^2)  
  
  # Likelihood portion for data set 2: zero-truncated counts
  for (j in 1:nsites2){
    C2[j] ~ dpois(lambda2[j])T(1,)
    log(lambda2[j]) <- alpha + beta * selev2[j]
  }
}