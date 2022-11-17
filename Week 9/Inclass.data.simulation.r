###############################################
# In-Class Data Simulation for Random Effect
# variance components

  rm(list=ls())

#Setup Dimensions of the data
  n.sites <- 25
  n.years <- 20
  
#Setup known parameters  
  beta0 = log(100)
  beta1 = 0.06
  sigma = 0.1
  set.seed(2)
  epsilon = matrix(rnorm(n.sites * n.years, 0, sigma), nrow=n.sites, ncol=n.years)

#Setup covariate  
  year = 0:(n.years-1)
  year = 1:n.years
  y = matrix(NA, n.sites, n.years)
  lambda = y

#For loop over n.sites and n.years  
#i = site
#t = year

for(i in 1:n.sites){
  for(t in 1:n.years){
    lambda[i,t] = beta0 + beta1 * year[t] + epsilon[i,t]
    set.seed(14)
    y[i,t] = rpois(1, exp(lambda[i,t]))
  } #end of t loop
} #end i loop

#Examine outputs
  hist(y)
  hist(epsilon)
  matplot(t(y), type="l")

#Save workspace  
  save.image("Extra.Noise.Workspace.RData")
  
###############################################
# In-Class Data Simulation for Random Effect
# random slope coeficients

  rm(list=ls())

#Setup Dimensions of the data
  n.sites <- 25
  n.years <- 20
  
#Setup known parameters  
  beta0 = log(100)
  mu.beta1 = 0.06
  sigma.beta1 = 0.1
  set.seed(32)
  beta1 = rnorm(n.sites, mu.beta1, sigma.beta1)

#Setup covariate    
  year = 1:n.years
  y = matrix(NA, n.sites, n.years)
  lambda = y

  sites=1:n.sites
  
#For loop over n.sites and n.years  
#i = site
#t = year
  
for(i in 1:n.sites){
  for(t in 1:n.years){
    lambda[i,t] = beta0 + beta1[i] * year[t]
    set.seed(14)
    y[i,t] = rpois(1, exp(lambda[i,t]))
  } #end of t loop
 
} #end i loop

#Examine outputs  
  hist(y)
  matplot(t(y), type="l")
  hist(beta1)  

#Save Workspace
  save.image("Random.Slopes.Workspace.RData")
