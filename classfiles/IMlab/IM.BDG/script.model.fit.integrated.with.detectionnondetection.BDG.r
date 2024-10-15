
# Here, we will fit an integrated species distribution model for the count data, zero-truncated data, and the detection non-detection data

# Load packages
  library(rjags)
  library(bayesplot)
  color_scheme_set("viridis")

# Load data
  load("three.data.sets")
  load("det.nondet")
  

# Bundle data
  C1 = data[[1]]$Counts
  C2 = data[[2]]$ZTCounts
  y = det.nondet[,1:3]

# Initial values
  inits <- function(){list(beta0 = runif(1), beta1 = rnorm(1))}

  data.list <- list(C1 = C1, 
                    C2 = C2, 
                    y = y, 
                    nsites1 = length(C1),
                    nsites2 = length(C2),
                    nsites3 = nrow(y),
                    noccasions = ncol(y),
                    selev1 = data[[1]]$selev1, 
                    selev2 = data[[2]]$selev2,
                    selev3 = det.nondet$selev3
                    )

# Parameters monitored
  params <- c("beta0", "beta1","p")

# MCMC settings
  na <- 1000; ni <- 6000; nb <- 2000; nc <- 4; nt <- 4

# Setup the Model
  jm=jags.model(file="IM2.jags.BDG.r", data=data.list,
              n.chains=nc,
              n.adapt=na)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the modedl  
  post.IM2=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)
  save(post.IM2,file="post.IM2")
# load("post.IM2")

#Look at chains
  mcmc_trace(post.IM2)

#Fancy plot of posterior
  mcmc_areas(as.matrix(post.IM2))

##########################
# If we have fit the separate models to each data set, we can then plot posteriors for each separate model 
# and compare to the integrated model

  load("post.count1")
  load("post.count2")
  load("post.occu")

#plot intercept
  plot(density(post1[[1]][,1]),lwd=3,col=1,xlim=c(0.3,2),main="Posteriors of Intercept",
       ylim=c(0,20))
  lines(density(post2[[1]][,1]),lwd=3,col=2)
  lines(density(post3b[[1]][,1]),lwd=3,col=3)
  lines(density(post.IM2[[1]][,1]),lwd=4,col=4)
  legend("topright",lwd=3,col=c(1,2,3,4),legend=c("Posterior of Counts",
                                                  "Posterior of ZT Counts",
                                                  "Posterior of Det/Non-Det",
                                                  "Posterior of Integrated Model2"
  ))


#plot slope
  plot(density(post1[[1]][,2]),lwd=3,col=1,xlim=c(-5,-1),main="Posteriors of Slope",
       ylim=c(0,15))
  lines(density(post2[[1]][,2]),lwd=3,col=2)
  lines(density(post3b[[1]][,2]),lwd=3,col=3)
  lines(density(post.IM2[[1]][,2]),lwd=4,col=4)
  legend("topright",lwd=3,col=c(1,2,3,4),legend=c("Posterior of Counts",
                                                  "Posterior of ZT Counts",
                                                  "Posterior of Det/Non-Det",
                                                  "Posterior of Integrated Model2"
                                                  ))
