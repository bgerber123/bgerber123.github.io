#################################
# Estimation of survival probabilities using capture-recapture data
# Implementing the cormack-jolly-seber model
#################################

library(stringr)
library(rjags)
library(coda)

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
  ni <- 5000 # number of iterations
  nt <- 2     # number of iterations to thin by
  nb <- 1000  # number of iterations to burn (to toss out initially)
  na <- 1000  # number of iterations to use to adapt to sample efficiently
  nc <- 3     # number of chains

# Call JAGS from R 
# Setup the Model
  jm=jags.model(file="cjs1.r", data=jags.data,n.chains=nc,n.adapt=na,inits=inits)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the model
  post=coda.samples(jm, variable.names=parameters, n.iter=ni, thin=nt)

# save(post,file="post.cjs")  
# load("post.cjs")  
  
#Interpret the results  
  summary(post)  
  plot(post)
  coda::gelman.diag(post)

# Highest posterior density intervals  ('credible intervals')
  browseURL("https://mathematica.stackexchange.com/questions/173282/computing-credible-region-highest-posterior-density-from-empirical-distributio")  
  coda::HPDinterval(post[[1]], prob = 0.95)
# Quantile posterior intervals ('credible intervals')
  apply(post[[1]],2,quantile, prob=c(0.025,0.975))
########################################################  

# Lab Assignment
  
#  Create a Markdown file and compiled HTML showing the code
#  and results for the below steps. Make sure to add text to describe what 
#  you are doing, why, and the result.

  
# Step 1  
# Fit a CJS survival model that includes a sex effect on survival probability.
# Do this by adapting the jags model code in the file (cjs1.r) and implementation code that is above.

# Step 2
# Check that parameters have converged. Show evidence of this by plotting and calculating
# the gelman-rubin convergence diagnostic, i.e, function gelman.diag. Make comments about what the plots
# and diagnostic statistic is telling you. 


# Step 3
# Use the estimated posterior parameters of survival to derive the posterior distributions 
# for the probability of survival for males and females. Visualize these distributions on the same plot.

# Step 4
# Use Monte Carlo integration on the sex effect parameter to estimate the probability that the 
# the effect difference of male survival is less than female survival

