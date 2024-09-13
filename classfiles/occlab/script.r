# Bayesian Occupancy model- single species / single season


# Libraries
  library(rjags)

#Read data
  bunny=read.csv("detection_nondetection_bunny.csv")

#Look at the data
  head(bunny)

# First two columns are covariates
#  ShrubHabitat - indicator variable of in shrub (1) or not in shrub
#  Veg - proportion of vegetation coverage at a sitew

# We went to each  site two times to search for our bunny species
  
# Observed1 and Observed2 are indicate whether we saw the bunny (1)
# or did not observe the bunny.
  
# Let's fit the most basic model - where we estimate a constant site occupancy and detection
# probability. I.e., we assume no variation due to covariates or anything else.
  
  
# MODEL 1 - Base Model

# First setup the data  
  
  bunny.data = cbind(bunny$Observed1,  bunny$Observed2)
  n.sites = nrow(bunny.data)
  n.visits = ncol(bunny.data)

# Package the data into a list    
  data=list(
    y=bunny.data,
    n.sites=nrow(bunny.data),
    n.visits=ncol(bunny.data)
  )
  
# Settings for MCMC chains
  nchains<-3
  niter<-2000
  nburn<-500
  nthin<-1
  
# Tell JAGS which parameters to save
  params=c("p","psi")
  
# We do not always need to feed in initial values, but it can help; we always need to initialize z to match our data
  inits <- function(){list(z=apply(bunny.data, 1, max), p=runif(1), psi=runif(1))}
  
# Run adaptive phase- this makes things more efficient
  jm=jags.model(file="occ.model.base.JAGS.R", data=data, inits=inits, n.chains=nchains, n.adapt=2000)
  
# Run the burn-in portion of the model
  update(jm, n.iter=nburn)
  
# Sample from the posterior
  M1 = coda.samples(jm, variable.names=params, n.iter=niter, thin=nthin)
  
# Check out a summary
  summary(M1)
  
# Look at traceplots
  traceplot(M1)

# Gelman-rubin diagnostic - to evaluate whether the posterior distributions
# have converged. We want the point.est to be <1.1
  gelman.diag(M1, autoburnin=FALSE)

# Plot posteriors of parameters  
  plot(M1)  
    
# First list are each chain - 3
# IN each element is a matrix of posteriors
# 1 column for p and 1 column for psi

# Plot posterior distribution of p in first chain    
  plot(density(M1[[1]][,1]),lwd=2)

# Get quantiles of posterior of p
  quantile(M1[[1]][,1])

  # Get quantiles of posterior of psi
  quantile(M1[[1]][,2])
  
####################################################
# MODEL 2 - Base Model version 2
  params=c("a0","b0")
  
  inits <- function(){list(z=apply(bunny.data, 1, max), a0=rnorm(1), b0=rnorm(1))}
  
  jm=jags.model(file="occ.model.base2.JAGS.R", data=data, inits=inits, n.chains=nchains, n.adapt=2000)
  
# Run the burn-in portion of the model
  update(jm, n.iter=nburn)
  
# Sample from the posterior
  M2 = coda.samples(jm, variable.names=params, n.iter=niter, thin=nthin)
  
# Check out a summary
  summary(M2)
  
#What are these parameters
  hist(plogis(M2[[1]][,1]))
  hist((M1[[1]][,1]),add=TRUE,col=2)
    
  