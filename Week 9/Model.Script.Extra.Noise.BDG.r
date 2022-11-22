#Setup the workspace
  rm(list=ls())
  library(rjags)
  library(bayesplot)
  library(runjags)
  library(coda)
  
#Load simulation data and setup
  load("Extra.Noise.Workspace.RData")

#Setup data and model for JAGS  
  
jags.data <- list(y=y,
                   n.sites=n.sites,
                   n.years=n.years,
                   year=year)

# Parameters monitored
  params <- c("beta0","beta1","sigma.epsilon") 

# MCMC settings 1
  n.adapt <- 100
  n.iter <- 1000
  n.thin <- 1
  n.burn <- 1
  n.chains <- 3

# MCMC settings 2
  n.adapt <- 5000
  n.iter <- 5000
  n.thin <- 2
  n.burn <- 2500
  n.chains <- 3
  
  inits <- function() {list(beta0=rnorm(1,0,1),
                            beta1=rnorm(1,0,0.1),
                            sigma.epsilon=runif(1,0.001,0.05))
                            }  
  
# Setup the Model
  jm=jags.model(file="model.JAGS.extra.noise.r", 
                data=jags.data,n.chains=n.chains,
                n.adapt=n.adapt,
                inits=inits)

# Update the model with the burnin
  update(jm, n.iter=n.burn)
  
#Fit the model
  post=coda.samples(jm, variable.names=params, 
                    n.iter=n.iter, thin=n.thin)

#save the model
  save("post",file="post.extra.noise")
  #load("post.extra.noise")

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
  mcmc_areas(as.matrix(post),pars=c("beta0","beta1","sigma.epsilon"))
  
#Fancy plot of posterior
  mcmc_areas(as.matrix(post),pars=c("beta1"))
  
#Combine chains and do regular r plots    
  post2=data.frame(combine.mcmc(post))

  hist(post2$beta0,xlim=c(4,5))
  abline(v=beta0,lwd=3)

  hist(post2$beta1)
  abline(v=beta1,lwd=3)
  
  hist(post2$sigma.epsilon,xlim=c(0,0.2))
  abline(v=sigma,lwd=3)
    
#Posterior mean and median
  apply(post2,2,median)
  
#95% Credible Intervals
  apply(post2,2,quantile, probs=c(0.025,0.975))

#######################################
#Predict
post.pred.data=post.pred=matrix(0,nrow=nrow(post2),ncol=n.years)  
for(t in 1:n.years){
  post.pred[,t]=  exp(post2$beta0+post2$beta1*year[t])
  post.pred.data[,t]= rpois(nrow(post2),exp(post2$beta0+post2$beta1*year[t]+post2$sigma.epsilon))
}

post.medians=apply(post.pred,2,median)
post.quants=apply(post.pred,2,quantile, probs=c(0.025,0.975))

plot(year,post.medians,type="l",lwd=3,ylim=c(0,400))
lines(year,post.quants[1,],col=2,lty=2,lwd=3)
lines(year,post.quants[2,],col=2,lty=2,lwd=3)  

matplot(t(y),add=TRUE,pch=1,col=1)


#######################
#Plot posterior data predictions
post.medians=apply(post.pred.data,2,median)
post.quants=apply(post.pred.data,2,quantile, probs=c(0.025,0.975))

plot(year,post.medians,type="l",lwd=3,ylim=c(0,400))
lines(year,post.quants[1,],col=2,lty=2,lwd=3)
lines(year,post.quants[2,],col=2,lty=2,lwd=3)  

matplot(t(y),add=TRUE,pch=1,col=1)

############################################

  head(post2)
  dim(post2)
  pred.mean=matrix(NA,nrow=n.years, ncol=nrow(post2))
  for(t in 1:n.years){
      pred.mean[t,] = exp(post2$beta0+post2$beta1*year[t])
  }

  dim(pred.mean)  
  hist(pred.mean[1,],xlim=c(100,150))
  
    hist(pred.mean[2,],add=TRUE,col=2)
  hist(pred.mean[3,],add=TRUE,col=3)
  hist(pred.mean[4,],add=TRUE,col=4)
  hist(pred.mean[5,],add=TRUE,col=5)
  
pred.mean.mean=  apply(pred.mean,1,mean)
  
pred.CI=apply(pred.mean,1,quantile, probs=c(0.025,0.975))
dim(pred.CI)
plot(year,pred.mean.mean,col=2,
     pch=18,cex=2,ylim=c(50,400))
lines(year,pred.CI[1,],col="green",cex=2,lwd=4)
lines(year,pred.CI[2,],col="green",cex=2,lwd=4)

matplot(t(y),pch=16,add=TRUE,col=1)

