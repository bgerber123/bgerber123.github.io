#Setup the workspace
  rm(list=ls())
  library(rjags)
  library(bayesplot)
  library(runjags)
  library(coda)
  
#Load simulation data and setup
  load("Random.Slopes.Workspace.RData")

#Setup data and model for JAGS  
  
dim(y)  

rows=sample(1:25,10,replace=TRUE)
cols=sample(1:20,10,replace=TRUE)

y[rows,cols]=NA


100/(25*20)

jags.data <- list(y=y,
                   n.sites=n.sites,
                   n.years=n.years,
                   year=year,
                   sites=sites)

# Parameters monitored
  params <- c("beta0","beta1","mu.beta1","sigma.beta1") 

# MCMC settings 1
  n.adapt <- 100
  n.iter <- 1000
  n.thin <- 1
  n.burn <- 1
  n.chains <- 3

# MCMC settings 2
  n.adapt <- 5000
  n.iter <- 2500
  n.thin <- 1
  n.burn <- 2500
  n.chains <- 3
  
  inits <- function() {list(beta0=rnorm(1,0,1),
                            mu.beta1=rnorm(1,0,0.1),
                            sigma.beta1=runif(1,0.001,0.05))
                            }  
  
# Setup the Model
  jm=jags.model(file="model.JAGS.random.slope.r", 
                data=jags.data,n.chains=n.chains,
                n.adapt=n.adapt,
                inits=inits)

# Update the model with the burnin
  update(jm, n.iter=n.burn)
  
#Fit the model
  post=coda.samples(jm, variable.names=params, 
                    n.iter=n.iter, thin=n.thin)

#save the model
  save("post",file="post.random.slope")
  #load("post.random.slope")
  
  
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
  mcmc_areas(as.matrix(post),pars=c("mu.beta1"))
  
  names=paste(rep("beta1[",n.sites),
  1:25,
  rep("]",n.sites),sep="")  
  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c(names))
  
#Combine chains and do regular r plots    
  post2=data.frame(combine.mcmc(post))

