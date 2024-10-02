##################

#Look at model notation first

##################
#Setup the workspace
rm(list=ls())
library(bayesplot)
library(rjags)
library(brms)

# Rabbit Occurrence Data
dat = read.csv("rabbit.occ.data.csv")

head(dat)

dat$dist.human=dat$dist.human/1000



#####################################
#####################################
# Fit the model in JAGS 

#JAGS data list
data = list(
  y = dat$occur,
  N = length(dat$occur),
  dist.human = dat$dist.human
)

#MCMC inputs  
n.chains=3
n.adapt=4000
n.iter=15000
thin=3
burn=4000

# Model Parameters to save values of
parms=c("b0","b1","sigma")	

# Setup the Model
jm=jags.model(file="model.jags.extra.variance.r", data=data,n.chains=n.chains,n.adapt=n.adapt)

# Update the model with the burnin
update(jm, n.iter=burn)

#Fit the modedl  
post=coda.samples(jm, variable.names=parms, n.iter=n.iter, thin=thin)

#save(post,file="post.extra.var")
#load("post")

#Look at chains
#Plot all chains MCMC iterations
color_scheme_set("viridis")
mcmc_trace(post)

#Fancy plot of posterior
mcmc_areas(as.matrix(post))

#Posterior mean and median
apply(as.matrix(post),2,mean)
apply(as.matrix(post),2,median)


