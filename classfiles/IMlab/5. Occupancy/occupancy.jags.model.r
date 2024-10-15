model {
  
  # Priors
  beta0 ~ dunif(-10, 10)   
  beta1 ~ dnorm(0, 1/10^2)  
  p ~ dbeta(1,1)
  
  # Likelihood portion for data set 3: detection/nondetection
  for (j in 1:nsites3){
    z[j]  ~ dbern(psi[j])
    cloglog(psi[j]) <- beta0 + beta1 * selev3[j]
    
    for(k in 1:noccasions){
        y[j,k] ~ dbern(p*z[j])
    }
  }
}