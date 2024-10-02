model {
  
  # Priors
  alpha ~ dunif(-10, 10)   
  beta ~ dnorm(0, 1/10^2)  
  
  # Likelihood portion for data set 3: detection/nondetection
  for (k in 1:nsites3){
    y[k] ~ dbern(psi[k])
    cloglog(psi[k]) <- alpha + beta * selev3[k]
  }
  
}