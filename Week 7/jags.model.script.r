#Setup the workspace
  rm(list=ls())
  library(rjags)
  library(bayesplot)

# Hippo survival data 
  y=c(0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)

#JAGS data list
  data=list(
            y=y,
            N=length(y)
            )

#MCMC inputs  
  n.chains=2
  n.adapt=100
  n.iter=1000
  thin=2
  burn=500

# Model Parameters to save values of
  parms=c("p")	


# Setup the Model
  jm=jags.model(file="model.jags.r", data=data)

# Update the model with the burnin
  update(jm, n.iter=burn,n.adapt=n.adapt)
  
#Fit the modedl  
  post=coda.samples(jm, variable.names=parms, n.chains=n.chains,n.iter=n.iter, thin=thin)

#Basic Plot of posterior
  hist(as.matrix(post))

#Look at chains
  #Plot all chains MCMC iterations
  color_scheme_set("blue")
  mcmc_trace(post)
  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post))

#Posterior mean and median
  mean(as.matrix(post))
  median(as.matrix(post))    

#95% Credible Intervals
  quantile(as.matrix(post),probs=c(0.025,0.975))


    