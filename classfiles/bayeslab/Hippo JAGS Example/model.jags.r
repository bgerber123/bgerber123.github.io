
model {

# Priors
  p ~ dbeta(1,1)

# Likelihood
  for (i in 1:N) {
     y[i] ~ dbern(p)        
  } 
}

