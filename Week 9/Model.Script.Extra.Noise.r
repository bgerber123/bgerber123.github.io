#Setup the workspace
  rm(list=ls())
  library(rjags)
  library(bayesplot)
  library(runjags)
  library(coda)
  
#Load simulation data and setup
  load("Extra.Noise.Workspace.RData")

#Setup data and model for JAGS  
  
jags.data <- list(y=y,
                   n.sites=n.sites,
                   n.years=n.years,
                   year=year)

# Parameters monitored
  params <- c("beta0","beta1","sigma.epsilon") 

# MCMC settings 1
  n.adapt <- 100
  n.iter <- 1000
  n.thin <- 1
  n.burn <- 1
  n.chains <- 3

# MCMC settings 2
  n.adapt <- 5000
  n.iter <- 5000
  n.thin <- 2
  n.burn <- 2500
  n.chains <- 3
  
  inits <- function() {list(beta0=rnorm(1,0,1),
                            beta1=rnorm(1,0,0.1),
                            sigma.epsilon=runif(1,0.001,0.05))
                            }  
  
# Setup the Model
  jm=jags.model(file="model.JAGS.extra.noise.r", 
                data=jags.data,n.chains=n.chains,
                n.adapt=n.adapt,
                inits=inits)

# Update the model with the burnin
  update(jm, n.iter=n.burn)
  
#Fit the model
  post=coda.samples(jm, variable.names=params, 
                    n.iter=n.iter, thin=n.thin)

#save the model
  save("post",file="post.extra.noise")
  #load("post.extra.noise")

#Diagnostics
  gelman.diag(post)
  gelman.plot(post)

#Basic traceplots  
  plot(post)
  
#Fancy traceplots
  #Plot all chains MCMC iterations
  color_scheme_set("blue")
  mcmc_trace(post)
  
#Post is a list of n.chains with a matrix of posterior samples  
  head(post[[1]])

  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c("beta0","beta1","sigma.epsilon"))
  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c("beta1"))
  
#Combine chains and do regular r plots    
  post2=data.frame(combine.mcmc(post))

