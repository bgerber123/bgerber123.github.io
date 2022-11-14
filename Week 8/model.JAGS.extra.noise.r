#i = site
#t = year

model{

#Priors
  #Overdispersion
  tau.epsilon ~ dgamma(0.001,0.001)
  sigma.epsilon = pow(1/tau.epsilon,2)
#Coefs  
  beta0 ~ dnorm(0,1/(3)^2)
  beta1 ~ dnorm(0,1/(3)^2)

  for(i in 1:n.sites){
    for(t in 1:n.years){
      counts[i,t] ~ dpois(lambda[i,t])
      log(lambda[i,t]) <- beta0 + beta1 * year.cov[i,t] + epsilon[i,t]
      epsilon[i,t] ~ dnorm(0, tau.epsilon)
    }
  }
} 