##################

#Look at model notation first

##################
#Setup the workspace
  rm(list=ls())
  library(brms)
  library(bayesplot)
  library(rjags)

# Rabbit Occurrence Data
  dat = read.csv("rabbit.occ.data.csv")

  head(dat)

  dat$dist.human=dat$dist.human/1000

#####################################
#####################################
# Fit a GLM model in JAGS with extra variance.

  #JAGS data list
  data=list(
    y = dat$occur,
    N = length(dat$occur),
    dist.human = dat$dist.human,
    PA = sub('..', '', dat$PA),
    N.PA = length(unique(dat$PA))
  )
  
  #MCMC inputs  
  n.chains=3
  n.adapt=1000
  n.iter=5000
  thin=2
  burn=1000
  
  # Model Parameters to save values of
  parms=c("b0","b1","sigma","epsilon")	
  
  
  # Look at logistic prior
  samps = rlogis(100000, location = 0, scale = 1)
  plot(density(samps),lwd=3)
  # Look at logistic prior on probability scale
  plot(density(plogis(samps)),lwd=3)
  
  # Look at normal prior with large std. deviation
  samps = rnorm(100000,0,10)
  plot(density(samps),lwd=3)
  # Look at logistic prior on probability scale
  plot(density(plogis(samps)),lwd=3)
  
#TAKE-AWAY!!
# Priors are not invariant to transformations - in this case, the transformatino is the logit/expit
  
  # Setup the Model
  jm=jags.model(file="model.jags.extra.variance.PA.r", data=data,n.chains=n.chains,n.adapt=n.adapt)
  
  # Update the model with the burnin
  update(jm, n.iter=burn)
  
  #Fit the modedl  
  post=coda.samples(jm, variable.names=parms, n.iter=n.iter, thin=thin)
  
  #save(post,file="post.extra.var.PA")
  #load("post.extra.var.PA")
  
  #Look at chains
  #Plot all chains MCMC iterations
  color_scheme_set("viridis")
  mcmc_trace(post)
  
  #Fancy plot of posterior
  mcmc_areas(as.matrix(post))
  
  #Posterior mean and median
  apply(as.matrix(post),2,mean)
  apply(as.matrix(post),2,median)
  
  #95% Credible Intervals
  apply(as.matrix(post),2,quantile, probs=c(0.025,0.5,0.975))
  
