
#Setup the workspace
rm(list=ls())
library(rjags)
library(bayesplot)
####  The Data
y=c(0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)



#JAGS data list
data=list(
  y=y,
  N=length(y)
)

  n.chains=1
  n.adapt=100
  n.iter=1000
  thin=2
  burn=500


parms=c("p")	


jm=jags.model(file="model.jags.r", data=data, n.chains=n.chains, n.adapt=n.adapt)
update(jm, n.iter=burn)
post=coda.samples(jm, variable.names=parms, n.iter=n.iter, thin=thin)

#Fancy
mcmc_areas(as.matrix(post))

hist(as.matrix(post))
