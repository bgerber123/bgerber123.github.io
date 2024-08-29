##################

#For Lecture - start with writing the model on the board

##################
#Setup the workspace
rm(list=ls())
library(brms)

# Rabbit Occurrence Data
dat = read.csv("rabbit.occ.data.csv")

head(dat)

dat$dist.human=dat$dist.human/1000


# Fit the model using STAN via the brms R package
  brm.fit = brm(formula = occur ~ 1 + dist.human + (0+dist.human|PA),  
                data = dat, 
                family = bernoulli(link = "logit"),
                warmup = 1000, 
                iter = 5000, 
                chains = 3, 
                cores = 3,
                sample_prior = TRUE
                )



#See underlying model
  brm.fit$model

#See default priors
  get_prior(brm.fit)

#Get values of prior distributions
  draws = prior_draws(brm.fit)
  head(draws)
  plot(density(draws[,1]))


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
# Fit the same model in JAGS


#Setup the workspace
  library(rjags)
  library(bayesplot)

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
  jm=jags.model(file="model.jags.r", data=data,n.chains=n.chains,n.adapt=n.adapt)

# Update the model with the burnin
  update(jm, n.iter=burn)
  
#Fit the modedl  
  post=coda.samples(jm, variable.names=parms, n.iter=n.iter, thin=thin)

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

