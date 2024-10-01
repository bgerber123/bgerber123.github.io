# Model fitting

load("three.data.sets")
library(rjags)
library(bayesplot)

# Initial values
inits <- function(){list(alpha = runif(1), beta = rnorm(1))}



# Bundle data
C1 = data[[1]]$Counts
C2 = data[[2]]$ZTCounts
y = data[[3]]$y

data.list <- list(C1 = C1, 
                  C2 = C2, 
                  y = y, 
                  nsites1 = length(C1),
                  nsites2 = length(C2),
                  nsites3 = length(y),
                  selev1 = data[[1]]$selev1, 
                  selev2 = data[[2]]$selev2,
                  selev3 = data[[3]]$selev3
                  )

# Parameters monitored
params <- c("mean.lam", "alpha", "beta")

# MCMC settings
na <- 1000; ni <- 6000; nb <- 2000; nc <- 4; nt <- 4

# Setup the Model
jm=jags.model(file="IM.jags.r", data=data.list,
              n.chains=nc,
              n.adapt=na)

# Update the model with the burnin
update(jm, n.iter=nb)

#Fit the modedl  
post=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)

#save(post,file="post.IM")
#load("post")

#Look at chains
#Plot all chains MCMC iterations
color_scheme_set("viridis")
mcmc_trace(post)

#Fancy plot of posterior
mcmc_areas(as.matrix(post))

#Posterior mean and median
apply(as.matrix(post),2,mean)
apply(as.matrix(post),2,median)

#95% Credible Intervals
apply(as.matrix(post),2,quantile, probs=c(0.025,0.5,0.975))

# Compare with brms mode fit  
apply(as.matrix(post)[,1:2],2,quantile, probs=c(0.025,0.5,0.975))
summary(brm.fit)$fixed

# Compare with brms mode fit  
quantile(as.matrix(post)[,8],probs=c(0.025,0.5,0.975))
summary(brm.fit)$random



