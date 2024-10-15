model {
  
  # Priors
  lambda ~ dgamma(0.001,0.001)
  p ~ dunif(0,1)
  
  # Likelihood portion for data set 1: counts
  for (i in 1:nsites1){
    N[i] ~ dpois(lambda)
    for (j in 1:noccs){  
      y[i,j] ~ dbinom(p,N[i])
    }
  }
  
}