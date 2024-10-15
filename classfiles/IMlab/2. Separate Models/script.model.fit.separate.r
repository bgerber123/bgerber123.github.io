# In Kery and Kellner, 2024 Chapter 20 they introduce three data sets. We will first look at the three types of data.
# Next, we will fit separate models to each dataset. Then, we will  fit an integrated model that jointly estimates the parameters.

# Load libraries
  library(VGAM)
  library(rjags)
  library(bayesplot)
  color_scheme_set("viridis")
  

# Load Data sets
  load("three.data.sets")

# Counts  
  C1 = data[[1]]

# Zero-truncated Counts  
  C2 = data[[2]]

# Presence-Absence
  y = data[[3]]
  

      
#List elements

# Counts with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
  head(C2)

# Zero truncated Counts with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
  head(C2)

# Presence Absence data with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
  head(y)

##############################################
##############################################
# Likelihood model fitting

# First, lets fit our Count data
  fm1 <- glm(Counts ~ selev1, 
             family = poisson(link = "log"),
             data=C1)
  coef(fm1)



#Second, fit Zero truncated data
  fm2 <- vglm(ZTCounts ~ selev2, 
              family = pospoisson(), 
              data = C2)
  coef(fm2)


#Third, fit presence absence data
  fm3 <- glm(y ~ selev3, 
             family = binomial(link = "cloglog"), 
             data = y)
  coef(fm3)


##############################################
##############################################
# Bayesian - model fitting separately

# Count Data 1

# Initial values
  inits <- function(){list(beta0 = runif(1), beta1 = rnorm(1))}

# Bundle data
  data.list <- list(C1 = C1, 
                    nsites1 = length(C1),
                    selev1 = data[[1]]$selev1
  )

# Parameters monitored
  params <- c("beta0", "beta1")

# MCMC settings
  na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
  jm=jags.model(file="./2. Separate Models/count1.jags.model.r", 
                data=data.list,
                n.chains=nc,
                n.adapt=na)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the model  
  post1=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)
  save(post1,file="post.count1")

#Look at chains
  mcmc_trace(post1)


##################################
# Fit the Count Data 2 (zero truncated)

# Initial values
  inits <- function(){list(beta0 = runif(1), beta1 = rnorm(1))}

# Bundle data
  data.list <- list(C2 = C2, 
                    nsites2 = length(C2),
                    selev2 = data[[2]]$selev2
  )

# Parameters monitored
  params <- c("beta0", "beta1")

# MCMC settings
  na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
  jm=jags.model(file="./2. Separate Models/ZTcount2.jags.model.r", 
                data=data.list,
                n.chains=nc,
                n.adapt=na)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the modedl  
  post2=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)
  save(post2,file="post.count2")

#Look at chains
  mcmc_trace(post2)



##################################  
# Fit the Presence - Absence data

# Initial values
  inits <- function(){list(beta0 = runif(1), beta1 = rnorm(1))}

# Bundle data
  data.list <- list(y = y, 
                    nsites3 = length(y),
                    selev3 = data[[3]]$selev3
  )

# Parameters monitored
  params <- c("beta0", "beta1")

# MCMC settings
  na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
  jm=jags.model(file="./2. Separate Models/presence.absence.jags.model.r", 
                data=data.list,
                n.chains=nc,
                n.adapt=na)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the modedl  
  post3=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)
  save(post3,file="post.PA3")


#Look at chains
#Plot all chains MCMC iterations
  mcmc_trace(post3)

######################################
######################################
# Compare posterior distributions of the intercept and slope
# Across each model

#If models were not fit, load posterior objects
 #load("post.count1")
 #load("post.count2")
 #load("post.PA3")

  head(post1[[1]])
  head(post2[[1]])
  head(post3[[1]])

  
#################  
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
#################  
  
# plot slope
  plot(density(post1[[1]][,2]),lwd=3,col=1,xlim=c(-2.5,-1),main="Posteriors of Slope")
  lines(density(post2[[1]][,2]),lwd=3,col=2)
  lines(density(post3[[1]][,2]),lwd=3,col=3)
  legend("topright",lwd=3,col=c(1,2,3,4),legend=c("Posterior of Counts",
                                                  "Posterior of ZT Counts",
                                                  "Posterior of Det/Non-Det"
  ))
  
# Include lines for the MLE
  abline(v=coef(fm1)[2],lwd=3,lty=3,col=1)
  abline(v=coef(fm2)[2],lwd=3,lty=3,col=2)
  abline(v=coef(fm3)[2],lwd=3,lty=3,col=3)
  