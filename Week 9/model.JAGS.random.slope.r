#i = site
#t = year

model{
  
#Priors
  #Across-Site (population level coefficients)
  beta0 ~ dnorm(0,1/3^2)
  mu.beta1 ~ dnorm(0,1/3^2)

  #Overdispersion
  sigma.beta1~dunif(0,3)
  tau.beta1 <- 1/pow(sigma.beta1,2)

#Likelihood
  for(i in 1:n.sites){
    for(t in 1:n.years){
      y[i,t] ~ dpois(lambda[i,t])
      log(lambda[i,t]) <- beta0 + beta1[sites[i]] * year[t] 
    }
      beta1[i]~dnorm(mu.beta1, tau.beta1)
    }
} 