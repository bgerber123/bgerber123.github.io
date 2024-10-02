model {
  
# Priors
  b0 ~ dlogis(0,1)
  b1 ~ dlogis(0,1)
  
#Alternative priors on the logit scale
#  b0 ~ dnorm(0,1/2.3^2)
#  b1 ~ dnorm(0,1/2.3^2)
# Note that JAGS parameters for the Normal distribution
# are mu (mean) and the precision, which is the inverse of the variance,
# i.e., tau = 1/sigma^2
  
# Likelihood
  for (i in 1:N) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + b1*dist.human[i]
  } #End loop
  
}