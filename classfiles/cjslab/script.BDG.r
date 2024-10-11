#################################
# Estimation of survival probabilities using capture-recapture data
# Implementing the cormack-jolly-seber model
#################################

library(stringr)
library(rjags)
library(coda)
library(runjags)

# We will use capture recapture data on the European Dipper which is
# available from Program MARK

# This data set is the same as that used in ‘Examples’ in Lebreton et al. (1992), 
# and consists of marking and recapture data from 294 breeding adults each year
# during the breeding period,from early March to 1 June. 
# All birds in the sample were at least 1 year old when initially banded.

#Get data
  dipper = read.table("DIPPER.INP", skip = 2,sep="", colClasses = "character")
  head(dipper)

#The number of individuals marked
  nrow(dipper)

# split column 1 into columns
  CH = matrix(as.integer(str_split_fixed(dipper[,1],"",7)),nrow=nrow(dipper))
  head(CH)
  
#  Sex variable: female = 1; male = 0
  sex = as.integer(dipper$V3)

# Create vector with occasion of marking for each individual (row of CH)
  get.first <- function(x) min(which(x!=0))
  f <- apply(CH, 1, get.first)

# Bundle data
  jags.data <- list(y = CH, f = f, nind = dim(CH)[1], n.occasions = dim(CH)[2])


# Initial values
# In JAGS we have to give good initial values for the latent state z. At all occasions when an individual was observed, its state is z = 1 for sure. 
# In addition, if an individual was not observed at an occasion, but was alive for sure, because it was observed before and thereafter 
#  (i.e. has a capture history of e.g. {101} or {10001}), then we know that the individual was alive at all of these occasions, and thus z = 1. 
#  Therefore, we should provide initial values of z = 1 at these positions as well. The following function provides such initial values from the observed capture histories:

  z.init <- matrix(NA, nrow = nrow(CH), ncol = ncol(CH))
  for(i in 1:dim(z.init)[1]){
    z.init[i, f[i]:dim(z.init)[2]] <- 1
    z.init[i,f[i]] <- NA
  }

# Create inits function  
  inits <- function(){list(phi = runif(1, 0, 1), p = runif(1, 0, 1), z = z.init)}

# Parameters monitored
  parameters <- c("phi", "p")

# MCMC settings
  ni <- 10000 # number of iterations
  nt <- 2     # number of iterations to thin by
  nb <- 5000  # number of iterations to burn (to toss out initially)
  na <- 1000  # number of iterations to use to adapt to sample efficiently
  nc <- 3     # number of chains

# Call JAGS from R 
# Setup the Model
  jm=jags.model(file="cjs1.r", data=jags.data,n.chains=nc,n.adapt=na,inits=inits)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the model
  post=coda.samples(jm, variable.names=parameters, n.iter=ni, thin=nt)
  
#Interpret the results  
  summary(post)  
  plot(post)
  coda::gelman.diag(post)

# Highest posterior density intervals  ('credible intervals')
  coda::HPDinterval(post[[1]], prob = 0.95)
# Quantile posterior intervals ('credible intervals')
  apply(post[[1]],2,quantile, prob=c(0.025,0.975))
########################################################  

# Lab Assignment  
  
#TO DO - BRIAN - make this a Markdown file

# Step 1  
# Fit a CJS survival model that includes a sex effect on survival probability.
# Adapt the jags model code in the other file and implementation code that is above.

# Parameters monitored
  parameters <- c("beta0","beta1", "p","male.phi","female.phi")
  
  inits <- function(){list(beta0 = rnorm(1),
                           beta1 = rnorm(1),
                           p = runif(1, 0, 1), 
                           z = z.init)}
  
  
# Bundle data
  jags.data <- list(y = CH, f = f, nind = dim(CH)[1], n.occasions = dim(CH)[2],
                    sex=sex)
  
# Setup the Model
  jm <- jags.model(file="cjs2.r", data=jags.data,n.chains=nc,n.adapt=na,inits=inits)
  
# Update the model with the burnin
  update(jm, n.iter=nb)
  
#Fit the model
  post2 <- coda.samples(jm, variable.names=parameters, n.iter=ni, thin=nt)
  
#Look at the results
  summary(post2)  
  
  
# Step 2
# Check that parameters have converged. Show evidence of this by plotting and calculating
# the gelman-rubin convergence diagnostic, i.e, function gelman.diag.

  plot(post2)
  gelman.diag(post2)
  
  
# Step 3
  
#We setup the JAGS model to derive the probability of survival. These are..  
  post.combined = runjags::combine.mcmc(post2)
  head(post.combined)

# Female and male survival probability  
  # Plot posteriors  
  plot(density(post.combined[,3]),lwd=3,col=1,"Annual Survival")
  lines(density(post.combined[,4]),lwd=3,col=2)  
  legend("topright",lwd=3,col=c(1,2),legend=c("Female","Maale"))
  
# Instead of asking JAGS to do it, we can use the estimated posterior parameters of survival to derive the posterior distributions 
# for the probability of survival for males and females. Visualize these distributions on the same plot.
  
  beta0 = post2[[1]][,1]
  beta1 = post2[[1]][,2]
  
  # Posteriors of survival of males and females  
  male.survival = plogis(beta0)
  female.survival = plogis(beta0+beta1)
  
  # Plot posteriors  
  plot(density(female.survival),lwd=3,col=1,"Annual Survival")
  lines(density(male.survival),lwd=3,col=2)  
  legend("topright",lwd=3,col=c(1,2),legend=c("Female","Male"))
  
  
  
# Step 4
# Use Monte Carlo integration on the sex effect parameter to estimate the probability that the 
# the effect difference of male survival is less than female survival

# The probability of a sex effect- that male survival is less than female surival
# To evaluate this, we want to know if beta1 is negative  
length(which(beta1<0))/length(beta1)
  
  

