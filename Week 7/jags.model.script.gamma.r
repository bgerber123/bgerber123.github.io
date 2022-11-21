
#Setup the workspace
rm(list=ls())
library(rjags)
library(bayesplot)

####  The Data
y=read.csv("counts.trees1.csv")[,1]
y2=read.csv("counts.trees2.csv")[,1]

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


parms=c("lam")	


jm=jags.model(file="model.jags.gamma.r", data=data, n.chains=n.chains, n.adapt=n.adapt)
update(jm, n.iter=burn)
post=coda.samples(jm, variable.names=parms, n.iter=n.iter, thin=thin)

#Fancy
mcmc_areas(as.matrix(post))

hist(as.matrix(post))


#############################
#Conjugate Poisson Likelihood with gamma prior

mean(y)
mean(y2)
#Gamma parameters
  #alpha = shape
  #beta = rate

#shape and rate
alpha=1
beta=1

curve(dgamma(x,shape=alpha,rate=beta),xlim=c(0,10))

alpha.star=alpha+sum(y)
beta.star=beta+length(y)

posterior1=rgamma(1000,shape=alpha.star,rate=beta.star)
hist(posterior1,freq=FALSE)

#####################################################
#shape and scale
k=alpha
theta=1/beta

curve(dgamma(x,shape=k,scale=theta),xlim=c(0,10))

k.star=k+sum(y)
theta.star=theta/(length(y)*theta+1)
posterior2=rgamma(1000,shape=k.star,scale=theta.star)

hist(posterior2,add=TRUE,freq=FALSE,col=2)



