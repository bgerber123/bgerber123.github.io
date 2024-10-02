
model {

# Priors
  b0 ~ dlogis(0,1)
  b1 ~ dlogis(0,1)
  sigma ~ dunif(0,5)
  tau <- 1/sigma^2
  
# Likelihood
  for (i in 1:N) {
     y[i] ~ dbern(p[i])        
     logit(p[i]) <- b0 + b1*dist.human[i]+epsilon[PA[i]]
  } #End loop
  
# Random Slope
  for(j in 1:N.PA){
    epsilon[j] ~ dnorm(0,tau)
  } #End loop
  
} #End Model


