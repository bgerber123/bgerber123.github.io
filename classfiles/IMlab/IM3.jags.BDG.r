model {
  
  # Priors and linear models: shared for models of all three data sets
  beta0 ~ dunif(-10, 10)                                 # Abundance intercept on log scale
  beta1 ~ dnorm(0, 1/10^2)                                # Slope on elevation
  p ~ dbeta(1,1)
  
  # Joint likelihood: Note identical alpha and beta for all data sets
  
  # Likelihood portion for data set 1: replicated counts
  for (i in 1:nsites1){
    N[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + beta1 * selev1[i]
    
    for (j in 1:noccasions1){  
      C1[i,j] ~ dbinom(p,N[i])
    }
  }
  
  
  # Likelihood portion for data set 2: zero-truncated counts
  for (j in 1:nsites2){
    C2[j] ~ dpois(lambda2[j])T(1,)
    log(lambda2[j]) <- beta0 + beta1 * selev2[j]
  }
  # Likelihood portion for data set 3: detection/nondetection
  for (j in 1:nsites3){
    z[j]  ~ dbern(psi[j])
    cloglog(psi[j]) <- beta0 + beta1 * selev3[j]
    
    for(k in 1:noccasions2){
      y[j,k] ~ dbern(p*z[j])
    }
  }#end site loop
} #End model
  