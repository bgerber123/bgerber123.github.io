##### Occupancy Module - NRS 520
### Creating grids of different sizes and sampling points within grid of different sizes
rm(list=ls())
library(rgeos)
library(sf)
library(raster)
library(runjags)


##### Make the sampling grids

## Grid 1 = small cells
# this is 100x100m simulation grid where each point is at 5m intervals
# corresponds to the centroid of each cell, or sampling location
x<-seq(from=0, to=100, by=5) #generates numbers 0-100 in increments of 5
y<-seq(from=0,to=100, by=5)
dat<-expand.grid(x=x, y=y) # puts all combinations of x and y together
dat<-SpatialPointsDataFrame(dat, coords = dat) # turn the coordinates into points
grid1<-points2grid(dat, tolerance=0.00587692) # turn those points into a grid
grid1<-as.data.frame(SpatialGrid(grid1)) 
grid1<-rasterFromXYZ(grid1) # we need this to be a working layer with ID for each, so make it a raster
grid1=rasterToPolygons(grid1) # and then turn that raster into polygons
grid1=st_as_sf(grid1) # turns it back into sf, see how we now have "layer" in there 
grid1$layer=seq(1:nrow(dat)) # assign a unique ID to each layer (polygon grid cell)
plot(dat)
plot(grid1, add=T, col=NA)


## Grid 2 = large cells
# make a second group that is larger centroids, so maybe 10m apart
# same code as above, we are just not increasing our nodes from 5 to 10m
x2<-seq(from=0, to=100, by=10)
y2<-seq(from=0,to=100, by=10)
dat2<-expand.grid(x=x2, y=y2)
dat2<-SpatialPointsDataFrame(dat2, coords = dat2)
grid2<-points2grid(dat2, tolerance=0.00587692)
grid2<-as.data.frame(SpatialGrid(grid2))
grid2<-rasterFromXYZ(grid2)
grid2=rasterToPolygons(grid2)
grid2=st_as_sf(grid2)
grid2$layer=seq(1:nrow(dat2))
plot(dat2)
plot(grid2, add=T, col=NA)

## Visualization
# first set the extent of the landscape then check out our sampling grids with centroids
e1<-extent(1,99,1,99)
e2<-extent(-1,101,-1,101)
plot(e1,col=NA,bty="n", axes=F )
plot(dat, add=T, col=2, pch=16)
plot(grid1, add=T, col=NA)
plot(e2, col=NA, bty="n", axes=F)
plot(dat2, add=T, col=3, pch=15) #our larger 
plot(grid2, add=T, col=NA)
# so our sampling grid is looking good! We have the following number of sites we 
# can randomly sample from for each scale since each layer has a unique ID associated
# with it:
length(dat) # so we have 441 cells in the more fine-scale grid
length(dat2) # and 121 in the larger grid


### Setting up the scenarios! 

# We have A) small HR, low intensity outbreak, B) small HR, high intensity outbreak, C) large HR, low intensity outbreak,
# D) large HR, high intensity outbreak
# REMEMBER - Home Range (HR) is equivalent to Dispersal Distance here in this case study
# Below are each of the 4 scenarios, just replace lines indicated in the for loop below to run the proper model
# DONT FORGET - you'll need to run it twice for each scenario beacuse you will need to do both grids (grid1 and grid2)

## Scenario A
# few points for small HR = low intensity outbreak

## Scenario B
# many points for small HR = high intensity outbreak

## Scenario C
# few points for large HR = low intensity outbreak

## Scenario D
# many points for large HR = high intensity outbreak

######################
# Great! Now we understand the background of our scenarios, 
# let's get some data and run the models

# First, we need to generate the dataset we are going to use
n.grid=nrow(grid1) # need to swap in grid2 after running with grid1
grid=grid1 # you'll need to swap in grid2 here after running with grid1
n.homerange=20 #change to: 20 (Scenario A and D), 100 (Scenario B), and 5 (Scenario C)
HRsize=2 # use 2 for small HR (Scenarios A and B) and 15 for large HR (Scenarios C and D)


# DO NOT NEED TO CHANGE ANYTHING BELOW
n.sim=5 # this is 5 different sets of home range locations that will represent 5
        # different sets of locations of prion distribution on the landscape
n.surveys=3 
zj=matrix(NA,nrow=n.grid,ncol=n.sim) #makes empty matrix to store our z values (true occupancy at each cell)
y=array(NA,dim=c(n.grid,n.surveys,n.sim)) #makes empty array to store our data for all cells, 3 survey occassions at each, and 1000 truths of each dataset 
coefs.est=matrix(NA, nrow=n.sim,ncol=3)
truth=vector(length=n.sim)

for(j in 1:n.sim){
  set.seed(1985+j)
  xcord<-runif(n.homerange, 0,100) #make x coordinates for home range centers
  set.seed(18915648+j)
  ycord<-runif(n.homerange, 0,100) # make y coordinates for home range centers
  HRcenters<-as.data.frame(cbind(xcord, ycord)) # combine coordinates
  colnames(HRcenters)<-c("x","y")
  HRcenters<-SpatialPointsDataFrame(HRcenters, coords = HRcenters) # make the coords a spatial layer
  HR<-gBuffer(HRcenters, width = HRsize, byid=TRUE) # creates the buffer around the HR center to serve as the circular home range 
  HR.2=st_as_sf(HR) # turns it into an sf object so we can run the intersect below  
  hr.list=st_intersects(HR.2,grid) 
  z=vector(length = n.grid) 
  zm=matrix(NA,n.grid,ncol=length(hr.list)) #sets up our z values
  for(i in 1:n.grid){
    for(m in 1:length(hr.list)){
      zm[i,m]=length(which(hr.list[[m]]==i))
    }
    z[i]=max(zm[i,])
  }
  zj[,j]=z
  alpha0=0.5 # Setting the true detection probability
  alpha1=-0.5 # setting the true coefficient for our covariate
  x1=rnorm(n = n.grid) #this is our covariate (soil binding potential) on detection
  logit.p=alpha0+alpha1*x1 # calcualting detection prob
  p.uncond=plogis(logit.p) 
  p=p.uncond*zj[,j]
  y1=rbinom(n=length(p),size=1,prob=p) #survey 1
  y2=rbinom(n=length(p),size=1,prob=p) #survey 2
  y3=rbinom(n=length(p),size=1,prob=p) #survey 3
  y[,,j]=cbind(y1,y2,y3) # combine all data for all 3 surveys
  
  model_text="model{
  for(i in 1:n.grid){
    for(j in 1:n.surveys){
  y[i,j]~dbern(p[i]*z[i])
  }
  z[i]~dbern(psi[i])
  logit(psi[i])=beta0
  logit(p[i])=alpha0+alpha1*x1[i]
  }
  beta0~dlogis(0,1)
  alpha0~dlogis(0,1)
  alpha1~dlogis(0,1)
  }"
  inits=function(){list(z=apply(y,1,max,na.rm=T),
                        alpha0=runif(1),
                        alpha1=runif(1),
                        beta0=runif(1))}
  JM=run.jags(model=model_text,
              monitor = c("alpha0","alpha1",
                          "beta0"),
              data=list(n.grid=n.grid,n.surveys=n.surveys,
                        y=y[,,j],x1=x1),
              burnin=1000,n.chains=3,sample=1000,
              method = "rjags",inits=inits
  )
  summary(JM)
  mean(z)
  
  coefs.est[j,]=summary(JM)[,4] #pull out the coefficients
  truth[j]=mean(zj[,j]) # calculate the true occupancy for each of the 5 different simulations
}

## Yay, let's check the estimates - you'll want to save these as 
## different objects for each model scenario you run (code it yourself!)
mean.est=apply(coefs.est,2,mean)
names(mean.est)=c("Psi Int","p Int", "p x1")
plogis(mean.est[1])
# estimates from the model
mean.est
# true occupancy
mean(truth)
# calculating bias
bias=plogis(mean.est[1])-mean(truth)
bias

### YOU'LL NEED TO CHANGE THESE EVERYTIME TO SAVE YOUR ESTIMATES
### FOR EACH SCENARIO
SAG1est<-mean.est #scenario A, grid 1 mean.estimates
SAG1truth<-mean(truth) #scenario A, grid 1 mean true occupancy
SAG1bias<-bias # scenario A, grid 1 bias

# Make table that combines all the scenario results together below