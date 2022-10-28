
model {

# Priors
  lam ~ dgamma(9,0.5)

# Likelihood
  for (i in 1:N) {
     y[i] ~ dpois(lam)        
  } 
}

