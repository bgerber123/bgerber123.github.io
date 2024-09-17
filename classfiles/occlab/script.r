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
  
################################  
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
  
#What are the parameters we estimates in M2...
  hist(plogis(M2[[1]][,1]))
  hist((M1[[1]][,1]),add=TRUE,col=2)

################################  
# MODEL 3 - Model with site-level covariates
  
  # Need to include are covaraite
  data=list(
    y=bunny.data,
    n.sites=nrow(bunny.data),
    n.visits=ncol(bunny.data),
    veg = bunny$Veg
  )
  
  
  params=c("a0","a1","b0","b1")
  
  inits <- function(){list(z=apply(bunny.data, 1, max), a0=rnorm(1), b0=rnorm(1),a1=rnorm(1), b1=rnorm(1))}
  
  jm=jags.model(file="occ.model.cov.JAGS.R", data=data, inits=inits, n.chains=nchains, n.adapt=2000)
  
  # Run the burn-in portion of the model
  update(jm, n.iter=nburn)
  
  # Sample from the posterior
  M3 = coda.samples(jm, variable.names=params, n.iter=niter, thin=nthin)
  
  # Check out a summary
  summary(M3)

 # Evaluate whether there is statistical support fot the effect of Veg on psi and p
  head(M3[[1]])

# alpha 1  - slope of Veg on detection probability
  alpha1=M3[[1]][,2]
  length(which(alpha1<0))/nrow(M3[[1]])
  quantile(alpha1)
  plot(alpha1,lwd=3)

  
  
# beta 1  - slope of Veg on psi
  beta1=M3[[1]][,4]
  length(which(beta1<0))/length(beta1)
  quantile(beta1)
  plot(density(beta1),lwd=3)
  
  
################################  
# MODEL 3 - Model with site-level covariates - via unmarked and then ubms
  
library(ubms)  
library(unmarked)  

UMF <- unmarkedFrameOccu(y=bunny.data,siteCovs=data.frame(veg = bunny$Veg))

model3.likelihood = occu(~veg ~veg, data=UMF)
summary(model3.likelihood)

# use R package to fit the same model in stan

model3.stan = stan_occu(~veg ~veg, data=UMF, chains=3, iter=2000)

model3.stan

model3.stan@stanfit


################################  

#CHALLENGE

# Step 1

# Ignore detection probability  and fit a Bayesian logistic regression model. 
# Use brm or JAGS
# To create this data, we sill combine our observations,

bunny.ignore.det = apply(bunny.data,1,sum)
bunny.ignore.det[which(bunny.ignore.det==2)]=1

# Now, we have site level observation without replication. A 1 indicates a detection in either column 1 or column 2 or both.
# A zero is no detection for other observation.

# Fit the model and estimate a slope for the effect of veg (bunny$veg)
bunny.ignore.det

# Compare this slope to your findings from your Bayesian occupancy model slopes - either model3.stan or M3. How are the results different?
# Think about the issue of ignoring detection probability and what this might mean for your interpretation of an ecological effect?

# Consider fitting this model in brms and loading the bayesplot package to plot posterior distributions
# Note that as_draws is a function in brms to extract posterior samples


# Step 2

# Use model3.stan or M3 to make a prediction plot of occupancy (y-axis) and veg (x-axis).


# Using ubms/stan

#The predict function works with ubms to get predictions

#We can use the predict function to get predictions of the 'state' or occurence probabiltiy
# Using Jags - need to backtransform parameters ourselves. There is no function to do this for us.
  
