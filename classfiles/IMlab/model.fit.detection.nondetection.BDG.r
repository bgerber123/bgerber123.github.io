# Model fitting

load("three.data.sets")

# Detection non-detection data with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
y =data[[3]]

#Turn into detection non-detection data
table(y$y)

#How many occasions/replicates per site?
k = 3

#Probability of detection per occasion, given occurence (z==1)
p = 0.2


#Create detection non-detection data
det.nondet = matrix(NA, nrow=length(y$y),ncol=k)

for(i in 1:length(y$y)){
  z = y$y[i]
  if(z==0){
    det.nondet[i,] = rep(0,k)
  }else{
      
    det.nondet[i,] = rbinom(k,1,p)
    }
}

head(det.nondet)
save(det.nondet,file="det.nondet")

#######################
#######################
# Likelihood
library(unmarked)

UMF <- unmarkedFrameOccu(y = det.nondet,
                         siteCovs = data.frame(selev3=y$selev3)
                         )

#Third, fit detection non-detection data
model.occu <- occu(~ 1 ~ selev3, linkPsi = "cloglog", data = UMF)

summary(model.occu)


# If we ignore detection

y.ignore = apply(det.nondet,1,sum)
y.ignore[which(y.ignore>0)]=1

model.occu.ignore.det <- glm(y.ignore ~ selev3, family = binomial(link = "cloglog"), data = data[[3]])

summary(model.occu.ignore.det)

#notice the change in the intercept and slope




#######################
#######################
# Bayesian - model fitting separately

library(rjags)
library(bayesplot)

##################################
##################################
# Detection - non-detection data

# Initial values
inits <- function(){list(alpha = runif(1), beta = rnorm(1))}

# Bundle data
y = det.nondet

data.list <- list(y = y, 
                  nsites3 = nrow(y),
                  selev3 = data[[3]]$selev3,
                  noccasions = ncol(y)
)

# Parameters monitored
params <- c("alpha", "beta","p")

# MCMC settings
na <- 2000; ni <- 10000; nb <- 4000; nc <- 4; nt <- 4

# Setup the Model
jm=jags.model(file="detection.nondetection.jags.model.r", data=data.list,
              n.chains=nc,
              n.adapt=na)

# Update the model with the burnin
update(jm, n.iter=nb)

#Fit the modedl  
post3b=coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)

# save(post3b,file="post.detection.nondetection")
# load("post.detection.nondetection")

#Look at chains
#Plot all chains MCMC iterations
color_scheme_set("viridis")
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
  
  