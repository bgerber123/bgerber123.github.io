# In chapter 20 of Kery and Kellner, 2024 they present three datasets of counts, zero-truncated counts, and presence-absences
# of the common swift,

# Here, we will consider the first dataset of counts, but as replicated counts. We will fit a N-mixture model
# to theses data to account for detection probability. 
# 

#Load packages
  library(unmarked)
  library(rjags)
  library(bayesplot)
  color_scheme_set("viridis")
  
# Load data
  load(file="count.rep")

  
# Look at data
  head(count.rep)  
  dim(count.rep)
# 500 sites with two replicates each  
  
#######################
#######################
# Likelihood Model
  
  selev1  = count.rep$selev1
  count.rep=data.frame(count.rep[,1],count.rep[,2])

# Setup unmarked model framework    
  UMF <- unmarkedFramePCount(y = count.rep,
                         siteCovs = data.frame(selev1=selev1)
                         )

#Third, fit N.mixture model using pcount function
# First ~ models detection and second ~ models Abundance  
  model.Nmixture <- pcount(~ 1 ~ selev1, data = UMF, K=150)
  coef(model.Nmixture)


# If we ignore detection and just take the first set of counts
  model.Counts.ignore.det <- glm(count.rep[,1] ~ selev1, family = poisson(link = "log"))
  coef(model.Counts.ignore.det)
# notice the change in the intercept

# If we ignore detection and just take the second set of counts
  model.Counts.ignore.det <- glm(count.rep[,2] ~ selev1, family = poisson(link = "log"))
  coef(model.Counts.ignore.det)
# notice the change in the intercept, but not the slope. This is because of assuming constatnt detection probability 
  


##############################################
##############################################
# N-mixture model - constant parameters (just a first step to ultiamtely get what we want- a model with covariates)
# Bayesian - model fitting 


# Initial values
  inits <- function(){list(p = runif(1), lambda = mean(unlist(count.rep)),
                           N = apply(count.rep,1,max))
    }

# Bundle data
  data.list <- list(y = count.rep, 
                    nsites1 = nrow(count.rep),
                    noccs = ncol(count.rep)
  )

# Parameters monitored
  params <- c("lambda", "p")

# MCMC settings
  na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
  jm=jags.model(file="./4 N.Mixture Model/n.mixture.jags.model.r", 
                data=data.list,
                inits=inits,
                n.chains=nc,
                n.adapt=na)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the model  
  post1b=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)
  save(post1b,file="post.count.rep")
# If not fitting the model, load saved objects with posterior samples
#  load("post.count.rep")

#Look at chains
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
  data.list <- list(y = count.rep, 
                    nsites1 = nrow(count.rep),
                    noccs = ncol(count.rep),
                    selev1 = selev1
  )

# Parameters monitored
  params <- c("beta0", "beta1","p")

# MCMC settings
  na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
  jm=jags.model(file="./4 N.Mixture Model/n.mixture.covs.jags.model.r", 
                data=data.list,
                inits=inits,
                n.chains=nc,
                n.adapt=na)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the model
  post1c=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)
  save(post1c,file="post.covs.count.rep")
  
# If not fitting the model, load saved objects with posterior samples
# load("post1c")

#Look at chains
  mcmc_trace(post1c)

# Examine posterior distributions
  summary(post1c[[1]])


  
