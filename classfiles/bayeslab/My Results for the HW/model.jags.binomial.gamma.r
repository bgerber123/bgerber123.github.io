
model {

  lambda ~ dgamma(prior.r, prior.lambda)

# Likelihood
  for (i in 1:N) {
     y[i] ~ dpois(lambda)        
  } 
}

