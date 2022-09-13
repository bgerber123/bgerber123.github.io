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
  setwd("C:/Users/bgerber/Google Drive/GITHUB/bgerber123.github.io/Week 2")

# Load shapefiles  
  RI=st_read("./extra/RI.Sq.Mile.shp")
  boundary=st_read("./extra/State_Boundary__1997_.shp")

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

# find values of high development  
  index=which(RI$Devel_area>0.8)

# drop values of high development and make a new spatial object
  RI=RI[-index,]

# Update the id column
  RI$Id=1:nrow(RI)
###################################  
###################################
#  Simulate deer densities
  set.seed(434343)
  deer1=rpois(nrow(RI),1)
  deer2=rpois(nrow(RI),20)
  

# Include these values in RI  
  RI$Deer1=deer1
  RI$Deer2=deer2

# Plot deer population by cell.
# This our TRUTH
  plot(RI["Deer1"])
  plot(RI["Deer2"])

# True simulated total population
  true.total1=sum(RI$Deer1)  
  true.total2=sum(RI$Deer2)  


# Evaluate 3 different sample sizes
  sample.sizes=c(5, 10, 20)
  n.sim=100

# Register how many processors to use (backend setup)
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)

# Start timer and foreach parallel loop with "dopar"  
  tic("Simulation Parallel")
#Define the ouput of the foreach loop
  out.parallel<-foreach(z=1:length(sample.sizes),
                      .packages = c("spsurvey"))%dopar% {

        deer.total.abundance=matrix(0,nrow=n.sim,ncol=2)
          for(i in 1:n.sim){  
            #Sample/model/predict deer
            eqprob <- grts(RI, n_base = sample.sizes[z])
            y1=eqprob$sites_base$Deer1
            y2=eqprob$sites_base$Deer2
            model1=glm(y1~1,family=poisson(link="log"))
            model2=glm(y2~1,family=poisson(link="log"))
            deer.total.abundance[i,1]=predict(model1,type="response")[1]*nrow(RI)
            deer.total.abundance[i,2]=predict(model2,type="response")[1]*nrow(RI)
          }
      #Output this object to the object out.parallel
      deer.total.abundance

  } #End foreach loop
toc() #End code timer

#stop procesors
  stopCluster(cl)

#Examine results  
  save(out.parallel,file="BDG.Results")
  
#If results are already run, load object  
#  load("BDG.Results")
  
  
  is.list(out.parallel)
  length(out.parallel)

#Smallest deer density    
  par(mfrow=c(3,1))
  hist(out.parallel[[1]][,1],xlim=c(0,3000),main="Deer Density of 1")
  abline(v=true.total1,col=3,lwd=4)
  hist(out.parallel[[2]][,1],xlim=c(0,3000),main="Deer Density of 1")
  abline(v=true.total1,col=3,lwd=4)
  hist(out.parallel[[3]][,1],xlim=c(0,3000),main="Deer Density of 1")
  abline(v=true.total1,col=3,lwd=4)
  
#largest deer density    
  par(mfrow=c(3,1))
  hist(out.parallel[[1]][,2],xlim=c(10000,26000),main="Deer Density of 20")
  abline(v=true.total3,col=3,lwd=4)
  hist(out.parallel[[2]][,2],xlim=c(10000,26000),main="Deer Density of 20")
  abline(v=true.total3,col=3,lwd=4)
  hist(out.parallel[[3]][,2],xlim=c(10000,26000),main="Deer Density of 20")
  abline(v=true.total3,col=3,lwd=4)
  
#restructure data and put the same deer densities together  
  deer.size1=cbind(out.parallel[[1]][,1],out.parallel[[2]][,1],out.parallel[[3]][,1])
  deer.size2=cbind(out.parallel[[1]][,2],out.parallel[[2]][,2],out.parallel[[2]][,2])
  
  deer1.allowed.error=true.total1*0.2
  deer2.allowed.error=true.total2*0.2
  
#Find how many samples are between the truth - error and truth + error  
  myfunc=function(total.est){
        length(which(total.est<true.total1+deer1.allowed.error & 
        total.est>true.total1-deer1.allowed.error))/length(total.est)
  }
  
  apply(deer.size1,2,FUN=myfunc)

#No sample size gets to within 95% of samples being within 20%  
  
#Find how many samples are between the truth - error and truth + error  
  myfunc2=function(total.est){
        length(which(total.est<true.total2+deer2.allowed.error & 
        total.est>true.total2-deer2.allowed.error))/length(total.est)
  }
  apply(deer.size2,2,FUN=myfunc2)
  
# All Sample sizes gets to high certainty within 20% of truth