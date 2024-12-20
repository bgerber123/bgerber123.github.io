# In Kery and Kellner, 2024 Chapter 20 they introduce three data sets. We will now fit an integrated model that jointly 
# considers each data set to estimate shared parameters.



# Load libraries 
  library(rjags)
  library(bayesplot)
  color_scheme_set("viridis")

  
# Load datasets  
  load("three.data.sets")

# Data objects
  C1 = data[[1]]$Counts
  C2 = data[[2]]$ZTCounts
  y = data[[3]]$y
  
# Covariate ocjects
  selev1 = data[[1]]$selev1
  selev2 = data[[2]]$selev2
  selev3 = data[[3]]$selev3
  
# Initial values
  inits <- function(){list(beta0 = runif(1), beta1 = rnorm(1))}


# Bundle data
  data.list <- list(C1 = C1, 
                    C2 = C2, 
                    y = y, 
                    nsites1 = length(C1),
                    nsites2 = length(C2),
                    nsites3 = length(y),
                    selev1 = selev1, 
                    selev2 = selev2,
                    selev3 = selev3
                    )

# Parameters monitored
  params <- c("beta0", "beta1")

# MCMC settings
  na <- 1000; ni <- 6000; nb <- 2000; nc <- 4; nt <- 4

# Setup the Model
  jm=jags.model(file="./3. Integrated Model/IM.jags.r", 
                data=data.list,
                n.chains=nc,
                n.adapt=na)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the model
  post.IM=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)
  save(post.IM,file="post.IM")

# If not fitting model, then load posterior object
# load("post.IM")
  
#Look at chains
  mcmc_trace(post.IM)

#Fancy plot of posterior
  mcmc_areas(as.matrix(post.IM))

##########################
# Load the separate model fits
  
 load("post.count1")
 load("post.count2")
 load("post.PA3")
  
  
# we can then plot posteriors for each separate model 
# and compare to the integrated model

par(mfrow=c(1,2))
 
#plot intercept
plot(density(post1[[1]][,1]),lwd=3,col=1,xlim=c(0.3,1),main="Intercept",
     ylim=c(0,30))
lines(density(post2[[1]][,1]),lwd=3,col=2)
lines(density(post3[[1]][,1]),lwd=3,col=3)
lines(density(post.IM[[1]][,1]),lwd=4,col=4)
legend("topright",lwd=3,col=c(1,2,3,4),legend=c("Posterior of Counts",
                                                "Posterior of ZT Counts",
                                                "Posterior of Det/Non-Det",
                                                "Posterior of Integrated Model"
))


#plot slope
plot(density(post1[[1]][,2]),lwd=3,col=1,xlim=c(-2.5,-1),main="Slope",
     ylim=c(0,15))
lines(density(post2[[1]][,2]),lwd=3,col=2)
lines(density(post3[[1]][,2]),lwd=3,col=3)
lines(density(post.IM[[1]][,2]),lwd=4,col=4)
legend("topright",lwd=3,col=c(1,2,3,4),legend=c("Posterior of Counts",
                                                "Posterior of ZT Counts",
                                                "Posterior of Det/Non-Det",
                                                "Posterior of Integrated Model"
                                                ))
