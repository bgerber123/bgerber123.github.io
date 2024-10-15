# In chapter 20 of Kery and Kellner, 2024 they present three datasets of counts, zero-truncated counts, and presence-absences
# of the common swift,

# Here, we will consider the third dataset of presence-absences, but as replicated detections (1) and non-detections (0). We will fit an 
# occupancy to theses data to account for detection probability. 
# 

#Load libraries
  library(unmarked)
  library(rjags)
  library(bayesplot)
  color_scheme_set("viridis")

# Load data
  load("det.nondet")

# Define data objects  
  selev3=det.nondet$selev3
  det.nondet=det.nondet[,1:3]
  
#######################
#######################
# Likelihood - model fitting with unmarked

# setup unmarked framework
  UMF <- unmarkedFrameOccu(y = det.nondet,
                           siteCovs = data.frame(selev3=selev3)
                           )

# Fit detection non-detection data with constant detection and occurence modeled with the variable
# selev3. We will use a cloglog link function to connect occurrence to mean count
  model.occu <- occu(~ 1 ~ selev3, linkPsi = "cloglog", data = UMF)
  coef(model.occu)


# Imagine, instead of modeling detection, we ignored it and combined our surveys and then assumed
# detection was 1.  
  y.ignore = apply(det.nondet,1,sum)
  y.ignore[which(y.ignore>0)]=1

  model.occu.ignore.det <- glm(y.ignore ~ selev3, family = binomial(link = "cloglog"), data = data[[3]])

  coef(model.occu.ignore.det)
# notice the change in the intercept and slope - not good!


##############################################
##############################################
# Bayesian - model fitting separately



# Bundle data
  data.list <- list(y = det.nondet, 
                    nsites3 = nrow(det.nondet),
                    selev3 = selev3,
                    noccasions = ncol(det.nondet)
  )

  
# Initial values
  inits <- function(){list(beta0 = runif(1), beta1 = rnorm(1))}
  
# Parameters monitored
  params <- c("beta0", "beta1","p")

# MCMC settings
  na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
  jm=jags.model(file="5. Occupancy/occupancy.jags.model.r", 
              data=data.list,
              n.chains=nc,
              n.adapt=na)

# Update the model with the burnin
  update(jm, n.iter=nb)

#Fit the modedl  
  post3b=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)
  save(post3b,file="post.occu")

# If not fitting the model, load the posteriors samples from the saved object
# load("post.occu")

#Look at chains
  mcmc_trace(post3b)


###################
# Examine posterior distributions
  head(post3b[[1]])

#plot intercept for psi
  plot(density(post3b[[1]][,1]),lwd=3,col=1,xlim=c(-1,3),main="Posteriors of Intercept")


#plot posterior distributions of the slope
  plot(density(post3b[[1]][,2]),lwd=3,col=1,xlim=c(0,-5),main="Posteriors of Slope")


#plot posterior distributions of p
  plot(density(post3b[[1]][,3]),lwd=3,col=1,xlim=c(0,0.4),main="Posteriors of Detection Prob")
  abline(v=p,lwd=3,col=2)
  
  