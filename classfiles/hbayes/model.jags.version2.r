
model {

# Priors
  b0 ~ dlogis(0,1)
  b1 ~ dlogis(0,1)
  sigma.b1 ~ dunif(0,5)
  tau.b1 <- 1/sigma.b1^2
  
# Likelihood
  for (i in 1:N) {
     y[i] ~ dbern(p[i])
     logit(p[i]) <- b0 + (b1 + b2[PA[i]])*dist.human[i]
  } #End loop
  
# Random Slope
  for(j in 1:N.PA){
    b2[j] ~ dnorm(0,tau.b1)
  } #End loop
  
} #End Model


