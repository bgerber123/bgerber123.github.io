
model {

# Priors
  b0 ~ dlogis(0,1)
  mu.b1 ~ dnorm(0, 3)
  sigma.b1 ~ dunif(0,5)
  tau.b1 <- 1/sigma.b1^2
  
# Likelihood
  for (i in 1:N) {
     y[i] ~ dbern(p[i])        
     logit(p[i]) <- b0 + b1[PA[i]]*dist.human[i]
  } #End loop
  
# Random Slope
  for(j in 1:N.PA){
    b1[j] ~ dnorm(mu.b1,tau.b1)
  } #End loop
  
} #End Model


