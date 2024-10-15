model {
  
  # Priors
  beta0 ~ dunif(-10, 10)   
  beta1 ~ dnorm(0, 1/10^2)  
  
  # Likelihood portion for data set 1: regular counts
  for (i in 1:nsites1){
    C1[i] ~ dpois(lambda1[i])
    log(lambda1[i]) <- beta0 + beta1 * selev1[i]
  }

}