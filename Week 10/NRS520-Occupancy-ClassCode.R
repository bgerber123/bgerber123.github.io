##### Occupancy Module - NRS 520
### Creating grids of different sizes and sampling points within grid of different sizes

library(rgeos)
library(sf)
library(raster)


# how to make a generic grid
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


# first set the extent of the landscape then check out our sampling grid
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
# These will be x2 because each will be with both grid sizes
# We have 1) small HR, dispersed, 2) small HR, concentrated, 3) large HR, dispersed,
# 4) large HR, concentrated

# few points for small, dispersed and more points for large concentrated
set.seed(543623)
xcord<-runif(20, 0,100) #selects 20 numbers between 0 and 100
set.seed(51594)
ycord<-runif(20, 0,100)
HRcentersA<-as.data.frame(cbind(xcord, ycord))
colnames(HRcentersA)<-c("x","y")
HRcentersA<-SpatialPointsDataFrame(HRcentersA, coords = HRcentersA)


# many points for small, concentrated
set.seed(543623)
xcord<-runif(100, 0,100) #selects 20 numbers between 0 and 100
set.seed(51594)
ycord<-runif(100, 0,100)
HRcentersB<-as.data.frame(cbind(xcord, ycord))
colnames(HRcentersB)<-c("x","y")
HRcentersB<-SpatialPointsDataFrame(HRcentersB, coords = HRcentersB)

# few points for large, dispersed
set.seed(543623)
xcord<-runif(5, 0,100) #selects 20 numbers between 0 and 100
set.seed(51594)
ycord<-runif(5, 0,100)
HRcentersC<-as.data.frame(cbind(xcord, ycord))
colnames(HRcentersC)<-c("x","y")
HRcentersC<-SpatialPointsDataFrame(HRcentersC, coords = HRcentersC)


# We have the centroids for each home range, now we need to create the buffer
# around those points that is going to serve as our circular HR. Small HR will
# have a buffer of 2m, and large will have a buffer of 15m
smallHRdisp<-gBuffer(HRcentersA, width = 2, byid = TRUE)
smallHRconc<-gBuffer(HRcentersB, width = 2, byid =TRUE)
largeHRdisp<-gBuffer(HRcentersC, width = 15, byid=TRUE)
largeHRconc<-gBuffer(HRcentersA, width = 15, byid=TRUE)
par(mfrow=c(2,2))
plot(e1,col=NA,bty="n", axes=F )
plot(smallHRdisp, add=T, col=3)
plot(e1,col=NA,bty="n", axes=F )
plot(smallHRconc, add=T, col=3)
plot(e2,col=NA,bty="n", axes=F )
plot(largeHRdisp, add=T, col=4)
plot(e2,col=NA,bty="n", axes=F )
plot(largeHRconc, add=T, col=4)



# put all 8 maps together 
par(mfrow=c(2,2), mar=c(1,1,2,1), cex=1)
#small grids, small HR, dispersed
plot(e1, xlab=NA, ylab=NA, main="small grids, small HR, dispersed", col=NA, bty="n", axes=F)
plot(smallHRdisp, add=T, col=5, border="black")
plot(dat, add=T, col=2, pch=16)
plot(grid1, add=T, col=NA)
# small grids, small HR, concentrated
plot(e1,xlab=NA, ylab=NA, main="small grids, small HR, concentrated", col=NA, bty="n", axes=F)
plot(smallHRconc, add=T, col=7, border="black")
plot(dat, add=T, col=2, pch=16)
plot(grid1, add=T, col=NA)
#large grids, small HR, dispersed
plot(e2,xlab=NA, ylab=NA, main="large grids, small HR, dispersed", col=NA, bty="n", axes=F)
plot(smallHRdisp, add=T, col=5)
plot(dat2, add=T, col=3, pch=15) 
plot(grid2, add=T, col=NA)
#large grids, small HR, concentrated
plot(e2, xlab=NA, ylab=NA, main="large grids, small HR, concentrated", col=NA, bty="n", axes=F)
plot(smallHRconc, add=T, col=7)
plot(dat2, add=T, col=3, pch=15)  
plot(grid2, add=T, col=NA)

#small grids, large HR, dispersed
plot(e1, xlab=NA, ylab=NA, main="small grids, large HR, dispersed", col=NA, bty="n", axes=F)
plot(largeHRdisp, add=T, col=5, border="black")
plot(dat, add=T, col=2, pch=16)
plot(grid1, add=T, col=NA)
# small grids, large HR, concentrated
plot(e1,xlab=NA, ylab=NA, main="small grids, large HR, concentrated", col=NA, bty="n", axes=F)
plot(largeHRconc, add=T, col=7, border="black")
plot(dat, add=T, col=2, pch=16)
plot(grid1, add=T, col=NA)
#large grids, large HR, dispersed
plot(e2,xlab=NA, ylab=NA, main="large grids, large HR, dispersed", col=NA, bty="n", axes=F)
plot(largeHRdisp, add=T, col=5)
plot(dat2, add=T, col=3, pch=15) 
plot(grid2, add=T, col=NA)
#large grids, large HR, concentrated
plot(e2, xlab=NA, ylab=NA, main="large grids, large HR, concentrated", col=NA, bty="n", axes=F)
plot(largeHRconc, add=T, col=7)
plot(dat2, add=T, col=3, pch=15)  
plot(grid2, add=T, col=NA)

# Great! Now we understand the background and creation all of our scenarios, 
# so let's run our occupancy model on the largeHR with the large grid 

# First, we need to generate the dataset we are going to use
n.iter=1000
zj=matrix(NA,nrow=121,ncol=n.iter)
y=array(NA,dim=c(121,3,n.iter))
for(j in 1:n.iter){
  xcord<-runif(20, 0,100) #selects 20 numbers between 0 and 100 (concentrated for large HR, or dispersed for small HR)
  ycord<-runif(20, 0,100)
  HRcenters<-as.data.frame(cbind(xcord, ycord))
  colnames(HRcenters)<-c("x","y")
  HRcenters<-SpatialPointsDataFrame(HRcenters, coords = HRcenters)
  largeHR<-gBuffer(HRcenters, width = 15, byid=TRUE) # this creates our large circular HR around the point with width of 15m (5m for small HR)
  largeHR.2=st_as_sf(largeHR) # turns that buffer into an sf object
  largehr.list=st_intersects(largeHR.2,grid2) # intersection of the grid and HR
  z=vector(length = length(grid2$layer))
  zm=matrix(NA,length(grid2$layer),ncol=length(largehr.list))
  for(i in 1:length(grid2$layer)){
    for(m in 1:length(largehr.list)){
      zm[i,m]=length(which(largehr.list[[m]]==i))
    }
    z[i]=max(zm[i,])
  }
  zj[,j]=z
  alpha0=0.5
  alpha1=-0.5
  x1=rnorm(n = length(grid2$layer))
  logit.p=alpha0+alpha1*x1
  expit=function(x){exp(x)/(1+exp(x))}
  p.uncond=expit(logit.p)
  p=p.uncond*zj[,j]
  y1=rbinom(n=length(p),size=1,prob=p)
  y2=rbinom(n=length(p),size=1,prob=p)
  y3=rbinom(n=length(p),size=1,prob=p)
  y[,,j]=cbind(y1,y2,y3)
}

head(y)
dim(y)

