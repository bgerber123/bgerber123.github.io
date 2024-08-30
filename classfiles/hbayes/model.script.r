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


# Fit the model using STAN via the brms R package
  brm.fit = brm(formula = occur ~ 1 + dist.human + (0 + dist.human|PA),  
                data = dat, 
                family = bernoulli(link = "logit"),
                warmup = 1000, 
                iter = 5000, 
                chains = 3, 
                cores = 3,
                sample_prior = TRUE
                )

#save(brm.fit,file="brm.fit")
#load("brm.fit")
  
#See underlying model
  brm.fit$model

#See default priors
  get_prior(brm.fit)

#Get values of prior distributions
  draws = prior_draws(brm.fit)
  head(draws)
  plot(density(draws[,1]),lwd=4)
  curve(dnorm(x,0,3),lwd=3,col=4,lty=3,add=TRUE)

# Note
# The b_Intercept parameter is this mean-centered intercept back-transformed to the original scale of the predictors
# Thus. "intercept' can be ignored
  brm.fit$fit
  summary(brm.fit)

#Extract 'fixed' and 'random effects'
  ranef(brm.fit)
  fixef(brm.fit)


#Trace Plots
  mcmc_plot(brm.fit, 
         type = "trace")

#All parameters and stuff
  names(brm.fit$fit)

#Posterior Distributions
  mcmc_areas(as.matrix(brm.fit),
           pars = names(brm.fit$fit)[c(1:3,5:9)],
           prob = 0.95)


#####################################
#####################################
# Fit the same model in JAGS (parameterized the same - Version 2; see pdf)

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
  parms=c("b0","b1","b2","sigma.b1")	
  
  
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
  jm=jags.model(file="model.jags.version2.r", data=data,n.chains=n.chains,n.adapt=n.adapt)
  
  # Update the model with the burnin
  update(jm, n.iter=burn)
  
  #Fit the modedl  
  post=coda.samples(jm, variable.names=parms, n.iter=n.iter, thin=thin)
  
  #save(post,file="post1")
  #load("post1")
  
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
  
# Compare with brms mode fit  
  apply(as.matrix(post)[,1:2],2,quantile, probs=c(0.025,0.5,0.975))
  summary(brm.fit)$fixed
  
# Compare with brms mode fit  
  quantile(as.matrix(post)[,8],probs=c(0.025,0.5,0.975))
  summary(brm.fit)$random

  
#####################################
#####################################
# Fit the same model in JAGS (parameterized Differently - version 1; see pdf)


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
  parms=c("b0","b1","mu.b1","sigma.b1")	

  
  # Setup the Model
  jm=jags.model(file="model.jags.version1.r", data=data,n.chains=n.chains,n.adapt=n.adapt)

# Update the model with the burnin
  update(jm, n.iter=burn)
  
#Fit the modedl  
  post=coda.samples(jm, variable.names=parms, n.iter=n.iter, thin=thin)

#save(post,file="post2")
#load("post2")
  
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

###################

