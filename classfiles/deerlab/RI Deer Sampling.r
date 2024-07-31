# Author: Brian D. Gerber
# Date Created: August, 29, 2022
# Updated last: 9/9/2022
# Objective: Evaluate sampling designs for counting 
#            white-tailed deer in Rhode Island (RI)


#Load Packages
  library(sf)
  library(ggplot2)
  library(spsurvey)
  library(foreach)
  library(doParallel)
  library(grDevices)
  library(tictoc)
  
# Set working directory and workinspace
  rm(list=ls())
  
# Load shapefiles  
  RI = st_read("./extra/RI.Sq.Mile.shp")
  boundary = st_read("./extra/State_Boundary__1997_.shp")

# Investigate the shapefile
  st_geometry(RI)
  class(RI)

  dim(RI)
  colnames(RI)

###################################  
###################################
# STEP TO CONSIDER: The Sampling Frame  
  
# Plot the boundary of RI and 1 square mile cells  
  ggplot() + 
    geom_sf(data = boundary, color = "purple",size=1) +
    geom_sf(data = RI,alpha = 0.5)

# Plot individual attributes
  plot(RI["Devel_area"])
#  plot(RI["water_area"])
#  plot(RI["Natur_area"])
#  plot(RI["Consv_area"])

# Consider the variation in developed area  
  hist(RI$Devel_area)

# Ask yourself are deer going to be at all development levels?  
# Should we further limit the sampling frame?
# Why/why not? 
  
# find values of high development  
  index=which(RI$Devel_area>0.8)

# drop values of high development and make a new spatial object
  RI=RI[-index,]

# plot updated map
  plot(RI["Devel_area"])

# Update the id column
  RI$Id=1:nrow(RI)
###################################  
###################################
#STEP TO CONSIDER: The TRUTH

# Consider how deer vary on the landscape
  
# Mean of 15 deer per square mile (1 cell)
  deer.dens=15

# Total expected deer population
  deer.dens*nrow(RI)

# Simulate deer densities
  set.seed(434343)
  deer1=rpois(nrow(RI),deer.dens)
  hist(deer1)
  sum(deer1)
  
# Include these values in RI  
  RI$Deer=deer1

# Plot deer population by cell.
# This our TRUTH
  plot(RI["Deer"])

# True simulated total population
  true.total=sum(RI$Deer)  
  true.total
  
# This is only one "realization" of the Poisson distribution. We are assuming this is truth.
# If we ran the code with a different seed value, we would get a new realization under the same
# mean parameter.
  
# Going forward, we are going to use design-based sampling to determine an appropriate sample size
  
###################################  
###################################
# STEP TO CONSIDER: Sampling Process

# Create a single sample
  eqprob <- grts(RI, n_base = 5)
  
# Plot this one sample  
  sp_plot(eqprob,RI,pch=18,cex=2)

###################################  
###################################
# STEP TO CONSIDER: Estimation

# For the single survey, extract the deer population by cell
  y=eqprob$sites_base$Deer
  
# Use this one sample to fit a model  
  mean(y)

###################################  
###################################
# STEP TO CONSIDER: Criteria to Evaluate
  
# We want to estimate total deer abundance
# It needs to be within 10% of truth
  
  
# predict total abundance
  total = mean(y)*nrow(RI)
  total
###################################  
###################################
# SIMULATION: Put it all together to evaluate. 
#            We want to find the sampling distribution of all possible 
#            estimates of Total Deer Abundance for a sample sizes of 5 and 10

# What sample sizes do we wish to consider?  
  sample.sizes=c(5, 10)
  
# How many repeat simulations should we do?
# More is usually better to apporximate the sampling disribution. 
# Keep it small to run the code fast.
  n.sim=400
  
# Setup the object we will save to: dimensions of sample.sizes by n.sim
  deer.total.abundance=matrix(0,length(sample.sizes),n.sim)

# Start code timer and Loop over sample size choices
  tic("simulation")
  for(z in 1:length(sample.sizes)){
  
  # For each sample size, repeat the 
  # sampling/estimation criteria n.sim times
    for(i in 1:n.sim){  
        set.seed(434343+i) #defined random number generation
        eqprob <- grts(RI, n_base = sample.sizes[z])
        y=eqprob$sites_base$Deer
        est=mean(y)
        deer.total.abundance[z,i]=est*nrow(RI)
    
      #monitor loops
      if(i%%10==0) cat("\nz =",z, ", i =", i)
    } #End i loop
  };toc() #End z loop and End codetimer
  
  
# First thing to do is save our results
  save(deer.total.abundance,file="deer.total.abundance")

# If already run and saved, load the results.  
  #load("deer.total.abundance")
  
# Let's look at our results for sample size n = 5
  hist(deer.total.abundance[1,],
       xlim=c(5000,25000), ylim=c(0,0.0005),
       main="Sampling Distribution of Total Deer Population",
       freq=FALSE,breaks=10
       )

# add sample size n= 10
  hist(deer.total.abundance[2,],
       freq=FALSE,breaks=10,
       add=TRUE,
       col=adjustcolor("red", alpha.f=0.5)
       )
  
# Add vertical line for truth  
  abline(v=true.total,col=4,lwd=3)

#Is our estimator/model/sampling process Statistically Biased?
  mean(deer.total.abundance[1,])-true.total
  mean(deer.total.abundance[2,])-true.total
  
  
# Relative Bias = (E[theta]  - theta)/theta
  (mean(deer.total.abundance[1,])-true.total)/true.total
  (mean(deer.total.abundance[2,])-true.total)/true.total

  
###################################  
#MONTE CARLO INTEGRATION
  
# Let's use our sampling distribution to see how good our ONE sample will be  
# We can ask questions about how likely it is to over estimate
# total abundance by 1000
  myfunc=function(total.est){
    length(which(total.est>true.total+1000))/length(total.est)
    }
  apply(deer.total.abundance,1,FUN=myfunc)
  
# We can ask what proportion of our estimates of total population are within 
# an error (lower/upper) of 1000
  myfunc2=function(total.est){
    length(which(abs(total.est-true.total)<1000))/length(total.est)
    }
  apply(deer.total.abundance,1,FUN=myfunc2)

#What are the worst estimates we could get (min, max); as a proportion of Truth
  apply(deer.total.abundance,1,FUN=range)/true.total
    
