#Setup the workspace
  rm(list=ls())
  library(rjags)
  library(bayesplot)
  library(runjags)
  library(coda)

setwd("C:/Users/bgerber/Google Drive/GITHUB/bgerber123.github.io/Week 8")
#Simulation setup 1 - time-series of counts with extra uncertainty

  n.sites=25
  n.years=20

  years=1:n.years
  beta0=4
  beta1=0.02
  counts.years=matrix(NA, nrow=n.sites,ncol=n.years)
  sigma=0.3
  set.seed(54345)
  epsilson=matrix(rnorm(n.sites*n.years,0,sigma),nrow=n.sites, ncol=n.years)
  for(i in 1:n.sites){
    lambda=exp(beta0+beta1*years+epsilson[i,])
    counts.years[i,]=rpois(n.years,lambda)
}


  hist(counts.years)
  matplot(t(counts.years),type="l")
#####################################################
#Fit model
  
year.cov=matrix(rep(years,n.sites),nrow=n.sites,byrow = TRUE)
year.cov[1,]
year.cov[2,]
  
 jags.data <- list(counts=counts.years,
                   n.sites=n.sites,
                   n.years=n.years,
                   year.cov=year.cov)


# Initial values
#  inits <- function() list( )


# Parameters monitored
  params <- c("beta0","beta1","sigma.epsilon") 

# MCMC settings
  n.adapt <- 2000
  n.iter <- 5000
  n.thin <- 2
  n.burn <- 2500
  n.chains <- 3

# Setup the Model
  jm=jags.model(file="model.JAGS.extra.noise.r", data=jags.data)

# Update the model with the burnin
  update(jm, n.iter=n.burn,n.adapt=n.adapt)
  
#Fit the modedl  
  post=coda.samples(jm, variable.names=params, n.chains=n.chains,n.iter=n.iter, thin=n.thin)

#Look at chains
  #Plot all chains MCMC iterations
  color_scheme_set("blue")
  mcmc_trace(post)
  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c("beta0","beta1","sigma.epsilon"))

#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c("beta1"))
  
  
#Posterior mean and median
  mean(as.matrix(post))
  median(as.matrix(post))    

#95% Credible Intervals
  quantile(as.matrix(post),probs=c(0.025,0.975))

########################################################
#Simulation setup 2 - time-series of counts with random effect slope

  n.sites=25
  n.years=20

  years=1:n.years

  beta0=4
  mu.beta1=0.02
  sigma.beta1=0.01
  set.seed(54353)
  beta1=rnorm(n.sites,mu.beta1,sigma.beta1)
  counts.years2=matrix(NA, nrow=n.sites,ncol=n.years)
  
  for(i in 1:n.sites){
    lambda=exp(beta0+beta1[i]*years)
    counts.years2[i,]=rpois(n.years,lambda)
  }

  counts.years2

  hist(counts.years2)

  matplot(t(counts.years2),type="l")

  num.na=10
  a=sample(1:n.sites,num.na,replace = FALSE)
  b=sample(1:n.years,num.na,replace = FALSE)
  counts.years2.NA=counts.years2
  counts.years2.NA[a,b]=NA
  
  
#############################
################
#Fit the second model

year.cov=matrix(rep(years,n.sites),nrow=n.sites,byrow = TRUE)
year.cov[1,]
year.cov[2,]
  
site=1:n.sites


 jags.data <- list(counts=counts.years2,
                   n.sites=n.sites,
                   n.years=n.years,
                   year.cov=year.cov,
                   site=site)


# Initial values
#  inits <- function() list( )


# Parameters monitored
  params <- c("beta0","beta1","mu.beta1","sigma.beta1") 

# MCMC settings
  n.adapt <- 2000
  n.iter <- 5000
  n.thin <- 2
  n.burn <- 2500
  n.chains <- 3

# Setup the Model
  jm=jags.model(file="model.JAGS.random.slope.r", data=jags.data)

# Update the model with the burnin
  update(jm, n.iter=n.burn,n.adapt=n.adapt)
  
#Fit the modedl  
  post=coda.samples(jm, variable.names=params, n.chains=n.chains,n.iter=n.iter, thin=n.thin)

#Look at chains
  #Plot all chains MCMC iterations
  color_scheme_set("blue")
  #mcmc_trace(post)
  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c("beta0","mu.beta1","sigma.beta1"))

#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c("mu.beta1"))

#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c("beta1[1]"))
  
#Fancy plot of posterior
  names=paste(rep("beta1[",n.sites),
  1:25,
  rep("]",n.sites),sep="")
  
  png(file="post.png",res=300,width=10,height=10,units="in")
  mcmc_areas(as.matrix(post),pars=names)
  dev.off()  
  
#Posterior mean and median
  mean(as.matrix(post))
  median(as.matrix(post))    

#95% Credible Intervals
  quantile(as.matrix(post),probs=c(0.025,0.975))

  post2=data.frame(combine.mcmc(post))

  head(post2)
  
  dim(post2)
  pred.mat=matrix(0,nrow=nrow(post2), ncol=n.years)
  for(t in 1:n.years){
    pred.mat=exp(post2$beta0  + post2$mu.beta1*years[t])
  }

    
  
  
  