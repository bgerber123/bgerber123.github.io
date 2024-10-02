# Model fitting

load("three.data.sets")

#List elements

# Counts with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
head(data[[1]])

# Zero truncated Counts with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
head(data[[2]])

# Detection non-detection data with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
head(data[[3]])


#######################
#######################
# Likelihood

# First, lets fit our Count data
fm1 <- glm(Counts ~ selev1, family = poisson(link = "log"),data=data[[1]])

summary(fm1)
exp(coef(fm1)[1])        


#Second, fit Zero truncated data
library(VGAM)
fm2 <- vglm(ZTCounts ~ selev2, family = pospoisson(), data = data[[2]])
summary(fm2)

exp(coef(fm2)[1])   


#Third, fit detection non-detection data
fm3 <- glm(y ~ selev3, family = binomial(link = "cloglog"), data = data[[3]])

summary(fm3)
exp(coef(fm3)[1])   





#######################
#######################
# Bayesian - model fitting separately

library(rjags)
library(bayesplot)

##################################
# Count Data 1

# Initial values
inits <- function(){list(alpha = runif(1), beta = rnorm(1))}



# Bundle data
C1 = data[[1]]$Counts

data.list <- list(C1 = C1, 
                  nsites1 = length(C1),
                  selev1 = data[[1]]$selev1
)

# Parameters monitored
params <- c("alpha", "beta")

# MCMC settings
na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
jm=jags.model(file="count1.jags.model.r", data=data.list,
              n.chains=nc,
              n.adapt=na)

# Update the model with the burnin
update(jm, n.iter=nb)

#Fit the model  
post1=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)

#save(post1,file="post.count1")
#load("post.count1")

#Look at chains
#Plot all chains MCMC iterations
color_scheme_set("viridis")
mcmc_trace(post1)


##################################
# Count Data 2

# Initial values
inits <- function(){list(alpha = runif(1), beta = rnorm(1))}

# Bundle data
C2 = data[[2]]$ZTCounts

data.list <- list(C2 = C2, 
                  nsites2 = length(C2),
                  selev2 = data[[2]]$selev2
)

# Parameters monitored
params <- c("alpha", "beta")

# MCMC settings
na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
jm=jags.model(file="ZTcount2.jags.model.r", data=data.list,
              n.chains=nc,
              n.adapt=na)

# Update the model with the burnin
update(jm, n.iter=nb)

#Fit the modedl  
post2=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)

#save(post2,file="post.count2")
#load("post.count2")

#Look at chains
#Plot all chains MCMC iterations
color_scheme_set("viridis")
mcmc_trace(post2)



##################################
# Detection - non-detection data

# Initial values
inits <- function(){list(alpha = runif(1), beta = rnorm(1))}

# Bundle data
y = data[[3]]$y

data.list <- list(y = y, 
                  nsites3 = length(y),
                  selev3 = data[[3]]$selev3
)

# Parameters monitored
params <- c("alpha", "beta")

# MCMC settings
na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
jm=jags.model(file="detection.jags.model.r", data=data.list,
              n.chains=nc,
              n.adapt=na)

# Update the model with the burnin
update(jm, n.iter=nb)

#Fit the modedl  
post3=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)

#save(post3,file="post.detection3")
#load("post.detection3")

#Look at chains
#Plot all chains MCMC iterations
color_scheme_set("viridis")
mcmc_trace(post3)


###################

# Compare posterior distributions of the intercept and slope

head(post1[[1]])
head(post2[[1]])
head(post3[[1]])

#plot intercept
  plot(density(post1[[1]][,1]),lwd=3,col=1,xlim=c(0.3,1),main="Posteriors of Intercept")
  lines(density(post2[[1]][,1]),lwd=3,col=2)
  lines(density(post3[[1]][,1]),lwd=3,col=3)
  legend("topright",lwd=3,col=c(1,2,3,4),legend=c("Posterior of Counts",
                                                  "Posterior of ZT Counts",
                                                  "Posterior of Det/Non-Det"
  ))
#Include lines for the MLE
  abline(v=coef(fm1)[1],lwd=3,lty=3,col=1)
  abline(v=coef(fm2)[1],lwd=3,lty=3,col=2)
  abline(v=coef(fm3)[1],lwd=3,lty=3,col=3)
  
#plot posterior distributions of the slopeslope
  plot(density(post1[[1]][,2]),lwd=3,col=1,xlim=c(-2.5,-1),main="Posteriors of Slope")
  lines(density(post2[[1]][,2]),lwd=3,col=2)
  lines(density(post3[[1]][,2]),lwd=3,col=3)
  legend("topright",lwd=3,col=c(1,2,3,4),legend=c("Posterior of Counts",
                                                  "Posterior of ZT Counts",
                                                  "Posterior of Det/Non-Det"
  ))
  
  #Include lines for the MLE
  abline(v=coef(fm1)[2],lwd=3,lty=3,col=1)
  abline(v=coef(fm2)[2],lwd=3,lty=3,col=2)
  abline(v=coef(fm3)[2],lwd=3,lty=3,col=3)
  