model {
  
  # Priors
  #BDG - check these
  beta0 ~ dnorm(0,0.01)
  beta1 ~ dnorm(0,0.01)
  
  p ~ dunif(0,1)
  
  # Likelihood portion for data set 1: counts
  for (i in 1:nsites1){
    N[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * selev1[i]
    
    for (j in 1:noccs){  
      y[i,j] ~ dbinom(p,N[i])
    }
  }
  
}