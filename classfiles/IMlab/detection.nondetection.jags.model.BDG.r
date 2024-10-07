model {
  
  # Priors
  alpha ~ dunif(-10, 10)   
  beta ~ dnorm(0, 1/10^2)  
  p ~ beta(1,1)
  
  # Likelihood portion for data set 3: detection/nondetection
  for (j in 1:nsites3){
    z[j]  ~ dbern(psi[j])
    cloglog(psi[j]) <- alpha + beta * selev3[j]
    
    for(k in 1:noccasions){
        y[j,k] ~ dbern(p*z[j])
    }
  }
}