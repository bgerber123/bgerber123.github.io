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
  n.chains=3
  n.adapt=1000
  n.iter=5000
  thin=2
  burn=1000

# Model Parameters to save values of
  parms=c("p")	


# Setup the Model
  jm=jags.model(file="model.jags.r", data=data,n.chains=n.chains,n.adapt=n.adapt)

# Update the model with the burnin
  update(jm, n.iter=burn)
  
#Fit the modedl  
  post=coda.samples(jm, variable.names=parms, n.iter=n.iter, thin=thin)

#Basic Plot of posterior
  hist(as.matrix(post))

#Look at chains
  #Plot all chains MCMC iterations
  color_scheme_set("viridis")
  mcmc_trace(post)
  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post))

#Posterior mean and median
  mean(as.matrix(post))
  median(as.matrix(post))    

#95% Credible Intervals
  quantile(as.matrix(post),probs=c(0.025,0.975))


    