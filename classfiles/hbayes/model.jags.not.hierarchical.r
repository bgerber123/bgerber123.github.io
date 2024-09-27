
model {

# Priors
  b0 ~ dlogis(0,1)
  b1 ~ dlogis(0,1)

# Likelihood
  for (i in 1:N) {
     y[i] ~ dbern(p[i])
     logit(p[i]) <- b0 + b1*dist.human[i]
  } #End loop
  
} #End Model


