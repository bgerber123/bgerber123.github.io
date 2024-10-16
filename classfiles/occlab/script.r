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

####################################################  
####################################################
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
  

####################################################  
####################################################
# MODEL 4 - Model with site-level covariates using design matrix notation
  
#Design matrix for occupancy  
  X = model.matrix(~bunny$Veg)
  
#Design matrix for detection
  W = model.matrix(~bunny$Veg)  

  # Need to include design matrices for detection and occupancy
  data=list(
    y=bunny.data,
    n.sites=nrow(bunny.data),
    n.visits=ncol(bunny.data),
    X=X,
    W=W,
    n.beta = ncol(X),
    n.alpha = ncol(W)
  )
  
  
  params=c("alpha","beta")
  
  inits <- function(){list(z=apply(bunny.data, 1, max), alpha=rnorm(ncol(W)),beta=rnorm(ncol(X)))}
  
  jm=jags.model(file="occ.model.cov.design.matrix.JAGS.R", data=data, inits=inits, n.chains=nchains, n.adapt=2000)
  
  # Run the burn-in portion of the model
  update(jm, n.iter=nburn)
  
  # Sample from the posterior
  M4 = coda.samples(jm, variable.names=params, n.iter=niter, thin=nthin)
  
  # Check out a summary
  summary(M4)
  
  # Evaluate whether there is statistical support for the effect of Veg on psi and p
  head(M4[[1]])
  
  # alpha 1  - slope of Veg on detection probability
  alpha1=M4[[1]][,2]
  length(which(alpha1<0))/nrow(M3[[1]])
  quantile(alpha1)
  plot(density(alpha1),lwd=3)

  # beta 1  - slope of Veg on psi
  beta1=M4[[1]][,4]
  length(which(beta1<0))/length(beta1)
  quantile(beta1)
  plot(density(beta1),lwd=3)  
  
  
    
####################################################  
####################################################
# MODEL 5 - Model with site-level covariates - via unmarked and then ubms
  
library(ubms)  
library(unmarked)  

UMF <- unmarkedFrameOccu(y=bunny.data,siteCovs=data.frame(veg = bunny$Veg))

model3.likelihood = occu(~veg ~veg, data=UMF)
summary(model3.likelihood)

# use R package to fit the same model in stan
  model5.stan = stan_occu(~veg ~veg, data=UMF, chains=3, iter=2000)
  model5.stan
  model5.stan@stanfit


####################################################  
####################################################
# Assignment 

  
##############
# Step 1

  
# Fit a Bayesian logistic regression model that ignores detection probability.
# Do this in JAGS or the brm R package
  
# The data we will use is the same as a above 'bunny.data', but we will combine the two columns
# This was common practice prior to the developement of the occupancy modeling approach
  
bunny.ignore.det = apply(bunny.data,1,sum)
bunny.ignore.det[which(bunny.ignore.det==2)]=1
bunny.ignore.det

# Now, we have site level observation without replication. A '1' indicates a detection in either column 1 or column 2 or both.
# A zero is no detection for other observation. We will assumes these are perfect presence / absence data 

# Fit the Bayesian logistic regression model and estimate a slope for the effect of veg (bunny$veg). Examine the traceplots and provide evidence
# that the posteriors have converged. 

# Compare this posterior distribution slope to your findings from your Bayesian occupancy model slope - either model3.stan or M3. Visualize these posteriors
# on the same plot or side-by-side plots. How are the results different?
# Think about the issue of ignoring detection probability and what this might mean when investigating ecological effects when not accounting
# for the observation process?

# If fitting this model in brms, use the bayesplot  package to plot posterior distributions.
# Note that 'as_draws' is a function in brms to extract posterior samples

##############
# Step 2

# Use the JAGS model with site-level covariates ('M3' or 'M4') or 'model3.stan' to make a prediction 
# plot of the probability of occupancy (y-axis) and veg (x-axis).
#
# If using the JAGS model results, make your predictions outside of JAGS and in R. See suggestions below.
#
# If using 'model3.stan' you need figre out how to use the 'predict' function to get what you want.
#
# Note the range of the Veg covariate to decide on the range of prediction values

range(bunny$Veg)

# Make your predictions b/w this range at an interval of 0.1. i.e

x.pred = seq(0,1,by=0.1)

# For each value in x.pred, you want to use your linear model....
# psi[i] <- plogis(b0+b1*veg[i])
#.... to derive the posterior distribution of psi for each value

# Remember, b0 is a posterior and b1 is a posterior. Combine the linear terms,
# then backtransform (logit-inverse) the combinations to the probability scale.

# Once you have a probability distribution of psi for each value of x.pred, think
# about how you might plot the full posterior for each value in a meaningful way, or summarize the posterior
# median and 95% highest posterior density intervals to then plot as lines.

