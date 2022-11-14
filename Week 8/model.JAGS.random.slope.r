#i = site
#t = year

model{
  
#Priors
  #Across-Site (population level coeficients)
  beta0 ~ dnorm(0,1/(3)^2)
  mu.beta1 ~ dnorm(0,1/(3)^2)
  tau.beta1 ~ dgamma(0.001,0.001)
  sigma.beta1 = pow(1/tau.beta1,2)
  for(i in 1:n.sites){
    for(t in 1:n.years){
      counts[i,t] ~ dpois(lambda[i,t])
      log(lambda[i,t]) <- beta0 + beta1[site[i]] * year.cov[i,t] 
    }
      beta1[i]~dnorm(mu.beta1, tau.beta1)
    }
} 