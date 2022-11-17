#i = site
#t = year

model{

#Priors
  #Overdispersion
  sigma.epsilon~dunif(0,3)
  tau.epsilon <- 1/pow(sigma.epsilon,2)

  #Coefs  
  beta0 ~ dnorm(0,1/3^2)
  beta1 ~ dnorm(0,1/3^2)

#Likelihood
  for(i in 1:n.sites){
    for(t in 1:n.years){
      y[i,t] ~ dpois(lam[i,t])
      log(lam[i,t]) <- beta0 + beta1 * year[t] + epsilon[i,t]
      epsilon[i,t] ~ dnorm(0, tau.epsilon)
    }
  }
} 