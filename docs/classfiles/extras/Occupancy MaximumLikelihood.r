########################################
#Author: Brian Gerber
#
# Objective: To connect basic probability to occupancy detection 
# histories to be used in maximum likelihood estimation
############################################

##########################################
# SETUP

# Imagine, we sample 10 sites and want to estimate species occurence probability.
# To correct for detection probability, we go to each site twice, to get replication

# The type of data we can observe for two replicates are

# 4 possible types of detection histories (1 = detection, 0 = not detected). 
#
# Det Hist 1 - 1 1
# Det Hist 2 - 1 0
# Det Hist 3 - 0 1
# Det Hist 4 - 0 0

# Here is our observed data
  y.hist=c(2,3,0,5)

#i.e., we observed detection history 1 ( 1 1 ) at 2 sites
#i.e., we observed detection history 2 ( 1 0 ) at 3 sites
#i.e., we observed detection history 3 ( 0 1 ) at 0 sites
#i.e., we observed detection history 4 ( 0 0 ) at 5 sites

# Our goal is to use the knowledge/patterns of detections at sites where we did
# observe the species (but imperfectly) to estimate the number of sites we did 
# not observe the species, but it was there (detection history 5)

# Let's define the likelihood as the product of the probabilities of observing each detection history.
# We will assume occupancy probability (psi) and detection probability (p) do not vary by site or occassion.

# E.g., the probability of observing detection history 1 (1 1) is: psi*p*p
# Why is this?
# Well, we know that the site is occupied so psi and we know we detected it on occasion 1 and occastion 2, so p*p.
# Putting it together, the joint probability of occurrence is detection is psi and p and p = psi * p * p

# Anotehr Example- the probability of observing detection history 4 (1 0) is: psi*p*(1-p)
# Why is this?
# Well, we know that the site is occupied so psi and we know we detected it on occasion 1 (p), but we did not detect it on occasion 2 (1-p)
# Putting it together, the joint probability of occurrence is detection is psi and p and 1-p = psi * p * (1-p)


# Our complete data likelihood for all combinations of possible detection histories is ....

likelihood.occ=function(y.hist,p,psi){
  (psi*p^2)^y.hist[1]   *  (psi*p*(1-p))^y.hist[2]   *  (psi*(1-p)*p)^y.hist[3]  *   ((1-psi)+psi*(1-p)*(1-p))^y.hist[4] 
}

#Let's Calculate the likelihood for one combinations of psi and p
  psi=0.6
  p=0.3

  likelihood.occ(y.hist=y.hist,psi=psi,p=p)

# WE need to do this same procedure for all/many possible combinations of possible psi and p  

#################
#Now take a guess at psi and p and calculate likelihood for each unique combinations
  psi=seq(0.01,0.99,0.01)
  p=seq(0.01,0.99,0.01)

  values=expand.grid(psi,p)

  likelihood=rep(0, dim(values)[1])

  for(i in 1:dim(values)[1]){
    likelihood[i]=likelihood.occ(y.hist=y.hist,psi=values[i,1],p=values[i,2])
  }


  results=cbind(values,likelihood)

# plot the likelihod surface
  require(akima) ; require(rgl)
  x=results[,1]
  y=results[,2]
  z=results[,3]
  s=interp(x,y,z)
  par(mfrow=c(2,1))
  image(s,xlab="Psi",ylab="p",main="Likelihood Surface")

# Get the maximum liklihood value
  which.max(results[,3])
  results[which.max(results[,3]),]

# We estimated psi as 0.61 (MLE) and p as 0.57 (MLE)
  
  
##############################################
