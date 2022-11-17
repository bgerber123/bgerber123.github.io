#Setup the workspace
  rm(list=ls())
  library(rjags)
  library(bayesplot)
  library(runjags)
  library(coda)
  
#Load simulation data and setup
  load("Random.Slopes.Workspace.RData")

#Setup data and model for JAGS  
  
jags.data <- list(y=y,
                   n.sites=n.sites,
                   n.years=n.years,
                   year=year,
                   sites=sites)

# Parameters monitored
  params <- c("beta0","beta1","mu.beta1","sigma.beta1") 

# MCMC settings 1
  n.adapt <- 100
  n.iter <- 1000
  n.thin <- 1
  n.burn <- 1
  n.chains <- 3

# MCMC settings 2
  n.adapt <- 5000
  n.iter <- 2500
  n.thin <- 1
  n.burn <- 2500
  n.chains <- 3
  
  inits <- function() {list(beta0=rnorm(1,0,1),
                            mu.beta1=rnorm(1,0,0.1),
                            sigma.beta1=runif(1,0.001,0.05))
                            }  
  
# Setup the Model
  jm=jags.model(file="model.JAGS.random.slope.r", 
                data=jags.data,n.chains=n.chains,
                n.adapt=n.adapt,
                inits=inits)

# Update the model with the burnin
  update(jm, n.iter=n.burn)
  
#Fit the model
  post=coda.samples(jm, variable.names=params, 
                    n.iter=n.iter, thin=n.thin)

#save the model
  save("post",file="post.random.slope")
  #load("post.random.slope")
  
  
#Diagnostics
  gelman.diag(post)
  gelman.plot(post)

#Basic traceplots  
  plot(post)
  
#Fancy traceplots
  #Plot all chains MCMC iterations
  color_scheme_set("blue")
  mcmc_trace(post)
  
#Post is a list of n.chains with a matrix of posterior samples  
  head(post[[1]])

  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c("mu.beta1"))
  
  names=paste(rep("beta1[",n.sites),
  1:25,
  rep("]",n.sites),sep="")  
  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c(names))
  
#Combine chains and do regular r plots    
  post2=data.frame(combine.mcmc(post))

  hist(post2$beta0,xlim=c(4,5))
  abline(v=beta0,lwd=3,col=2)
  curve(dnorm(x,0,3),lwd=3,col=4,add=TRUE)

#plot posterior and prior  
  plot(density(post2$mu.beta1),lwd=3)
  abline(v=mu.beta1,lwd=3,col=2)
  curve(dnorm(x,0,3),lwd=3,col=4,add=TRUE)
  
    
#Posterior mean and median
  apply(post2,2,median)
  
#95% Credible Intervals
  apply(post2,2,quantile, probs=c(0.025,0.975))
#######################################
#look at beta's
beta.post=post2[,-c(1,27,28)]
dim(beta.post)

png(file="betas.png",res=300,units="in",height=10,width=10)
par(mfrow=c(5,5))
for(i in 1:25){
  plot(density(beta.post[,i]),col=1,lwd=3,main=paste(c("beta",i),sep=""))
  abline(v=beta1[i],col=2,lwd=2)
}
dev.off()
#######################################
#Predict the mean for the average site
  
post.pred=array(0,dim=c(nrow(post2),n.years))  
for(t in 1:n.years){
  post.pred[,t]=  exp(post2$beta0+post2$mu.beta1*year[t])
}

post.medians=apply(post.pred,2,median)
post.quants=apply(post.pred,2,quantile, probs=c(0.025,0.975))

plot(year,post.medians,type="l",lwd=3,ylim=c(0,2000))
lines(year,post.quants[1,],col=2,lty=2,lwd=3)
lines(year,post.quants[2,],col=2,lty=2,lwd=3)  

matplot(t(y),add=TRUE,pch=1,col=1)

    
names(post2)

beta.post=post2[,-c(1,27,28)]
#Predict mean for each site  
post.pred.site.data=post.pred.site=array(0,dim=c(n.sites,n.years,nrow(post2)))  
for(i in 1:n.sites){
for(t in 1:n.years){
  post.pred.site[i,t,]=  exp(post2$beta0+beta.post[,i]*year[t])
  post.pred.site.data[i,t,]=  rpois(nrow(post2),post.pred.site[i,t,])
}
}

dim(post.pred.site)

post.medians=apply(post.pred.site,c(1,2),median)
post.quants=apply(post.pred,c(1,2),quantile, probs=c(0.025,0.975))

dim(post.medians)
dim(post.quants)

#posterior medians for each site
matplot(t(post.medians),type="l")
  
############################
#Predictions for site 1

dim(post.pred.site.data)
post.medians=apply(post.pred.site.data,c(1,2),median)
post.quants=apply(post.pred.site.data,c(1,2),quantile, probs=c(0.025,0.975))

dim(post.quants)
dim(post.medians)

site.index=2
plot(year,post.medians[site.index,],type="l",lwd=3,lty=3)
lines(year,post.quants[1,site.index,],lwd=3,col=2)
lines(year,post.quants[2,site.index,],lwd=3,col=2)
dim(y)
points(y[site.index,])

cor(y[site.index,],post.medians[site.index,])
