# Model fitting

load("three.data.sets")

# Convert the Count data into under detected counts with replication - suitable for an N-mixture model. 

abundance = data[[1]]


#Again, we need to consider the number of replicates and detection probability....

# How many occasions/replicates per site?
k = 2

# Probability of detection per occasion, given occurence (z==1)
p = 0.7


#Create replcaited count data
count.rep = matrix(NA, nrow=nrow(abundance),ncol=k)

for(i in 1:nrow(abundance)){
  set.seed(543534+i)
  count.rep[i,] = rbinom(k, abundance$Counts[i], p)
}

head(count.rep)
save(count.rep,file="count.rep")


#######################
#######################
# Likelihood
library(unmarked)

UMF <- unmarkedFramePCount(y = count.rep,
                         siteCovs = data.frame(selev3=abundance$selev1)
                         )

#Third, fit detection non-detection data
model.Nmixture <- pcount(~ 1 ~ selev3, data = UMF)

summary(model.Nmixture)


# If we ignore detection and just take the first set of counts
  model.Counts.ignore.det <- glm(count.rep[,1] ~ abundance$selev1, family = poisson(link = "log"))
  summary(model.Counts.ignore.det)
# notice the change in the intercept

# If we ignore detection and just take the second set of counts
  model.Counts.ignore.det <- glm(count.rep[,2] ~ abundance$selev1, family = poisson(link = "log"))
  summary(model.Counts.ignore.det)
# notice the change in the intercept
  


#######################
#######################
# N-mixture model - constant parameters
# Bayesian - model fitting 

library(rjags)
library(bayesplot)



# Initial values
inits <- function(){list(p = runif(1), lambda = mean(abundance$Counts),
                         N = apply(count.rep,1,max))
  }

# Bundle data
y = count.rep
data.list <- list(y = y, 
                  nsites1 = nrow(y),
                  noccs = ncol(y)
)

# Parameters monitored
params <- c("lambda", "p")

# MCMC settings
na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
jm=jags.model(file="n.mixture.jags.model.r", 
              data=data.list,
              inits=inits,
              n.chains=nc,
              n.adapt=na)

# Update the model with the burnin
update(jm, n.iter=nb)

#Fit the modedl  
post1b=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)

# save(post1b,file="post.count.rep")
# load("post.count.rep")

#Look at chains
#Plot all chains MCMC iterations
color_scheme_set("viridis")
mcmc_trace(post1b)


# Examine posterior distributions
head(post1b[[1]])

  
#######################
#######################
# N-mixture model - with site level variation 
# Bayesian - model fitting 

# Initial values
inits <- function(){list(p = runif(1), 
                         N = apply(count.rep,1,max),
                         beta0=rnorm(1),
                         beta1=rnorm(1))
}

# Bundle data
y = count.rep
data.list <- list(y = y, 
                  nsites1 = nrow(y),
                  noccs = ncol(y),
                  selev1 = abundance$selev1
)

# Parameters monitored
params <- c("beta0", "beta1","p")

# MCMC settings
na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
jm=jags.model(file="n.mixture.covs.jags.model.r", 
              data=data.list,
              inits=inits,
              n.chains=nc,
              n.adapt=na)

# Update the model with the burnin
update(jm, n.iter=nb)

#Fit the modedl  
post1c=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)

# save(post1c,file="post.covs.count.rep")
# load("post1c")

#Look at chains
#Plot all chains MCMC iterations
color_scheme_set("viridis")
mcmc_trace(post1c)


# Examine posterior distributions
head(post1c[[1]])


