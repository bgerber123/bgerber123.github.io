#Setup the workspace
  rm(list=ls())
  library(brms)
  library(bayesplot)

# Rabbit Occurrence Data
dat = read.csv("rabbit.occ.data.csv")

head(dat)

dat$dist.human=dat$dist.human/1000


# Fit the model using STAN via the brms R package
brm.fit = brm(formula = occur ~ 1 + dist.human,  
              data = dat, 
              family = bernoulli(link = "logit"),
              warmup = 1000, 
              iter = 5000, 
              chains = 3, 
              cores = 3,
              sample_prior = TRUE
)

# save(brm.fit,file="brm.fit.glm")
# load("brm.fit.glm")

# See underlying model
  brm.fit$model

# See default priors
  get_prior(brm.fit)

# Get values of prior distributions
  draws = prior_draws(brm.fit)
  head(draws)
  plot(density(draws[,1]),lwd=4)

# Note
# The b_Intercept parameter is this mean-centered intercept back-transformed to the original scale of the predictors
# Thus. "intercept' can be ignored
  brm.fit$fit
  summary(brm.fit)

#Extract 'fixed' and 'random effects'
  fixef(brm.fit)

#Trace Plots
  mcmc_plot(brm.fit, 
            type = "trace")

#All parameters and stuff
  names(brm.fit$fit)

#Posterior Distributions
mcmc_areas(as.matrix(brm.fit),
           pars = names(brm.fit$fit)[c(1,2)],
           prob = 0.95)
