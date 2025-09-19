## ----knitr, echo=FALSE,results='hide'---------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(magrittr)
library(knitr)
knitr::purl(input="../FW552/Cluster.qmd",output="../FW552/Proportions.r")


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
ponds = data.frame(matrix(nrow=6,ncol=2))
colnames(ponds)=c("Pond","egg.mass")

ponds$Pond=LETTERS[1:6]
ponds$egg.mass = c(2,6,8,10,10,12)
ponds$cluster = c(1,2,1,2,3,3)
ponds %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)



## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
cluster.df=NULL
cluster.df$clusterA= c(1,2,3)
cluster.df$member1 = c(2,6,10)
cluster.df$member2 = c(8,10,12)
cluster.df$mean = c(5,8,11)
cluster.df$dev2 = c(9,0,9)
cluster.df=data.frame(cluster.df)
cluster.df %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
cluster.df=NULL
cluster.df$clusterA= c(1,2,3)
cluster.df$member1 = c(2,6,10)
cluster.df$member2 = c(8,10,12)
cluster.df$mean = c(5,8,11)
cluster.df$dev2 = c(9,0,9)
cluster.df=data.frame(cluster.df)
cluster.df %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
ponds = data.frame(matrix(nrow=6,ncol=2))
colnames(ponds)=c("Pond","egg.mass")

ponds$Pond=LETTERS[1:6]
ponds$egg.mass = c(2,6,8,10,10,12)
ponds$clusterB = c(1,1,2,2,3,3)
ponds %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)



## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
cluster.df=NULL
cluster.df$clusterB= c(1,2,3)
cluster.df$member1 = c(2,8,10)
cluster.df$member2 = c(6,10,12)
cluster.df$mean = c(4,9,11)
cluster.df$dev2 = c(16,1,9)
cluster.df=data.frame(cluster.df)
cluster.df %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
cluster.df=NULL
cluster.df$clusterB= c(1,2,3)
cluster.df$member1 = c(2,8,10)
cluster.df$member2 = c(6,10,12)
cluster.df$mean = c(4,9,11)
cluster.df$dev2 = c(16,1,9)
cluster.df=data.frame(cluster.df)
cluster.df %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
ponds = data.frame(matrix(nrow=6,ncol=2))
colnames(ponds)=c("Pond","egg.mass")

ponds$Pond=LETTERS[1:6]
ponds$egg.mass = c(2,6,8,10,10,12)
ponds$clusterC = c(1,2,3,3,2,1)
ponds %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)



## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
cluster.df=NULL
cluster.df$clusterC= c(1,2,3)
cluster.df$member1 = c(2,6,8)
cluster.df$member2 = c(12,10,10)
cluster.df$mean = c(7,8,9)
cluster.df$dev2 = c(1,0,1)
cluster.df=data.frame(cluster.df)
cluster.df %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
cluster.df=NULL
cluster.df$clusterC= c(1,2,3)
cluster.df$member1 = c(2,6,8)
cluster.df$member2 = c(12,10,10)
cluster.df$mean = c(7,8,9)
cluster.df$dev2 = c(1,0,1)
cluster.df=data.frame(cluster.df)
cluster.df %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
ponds = data.frame(matrix(nrow=6,ncol=2))
colnames(ponds)=c("Pond","egg.mass")

ponds$Pond=LETTERS[1:6]
ponds$egg.mass = c(2,6,8,10,10,12)
ponds$clusterD = c(1,1,1,2,2,3)
ponds %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)



## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
cluster.df=NULL
cluster.df$clusterD= c(1,2,3)
cluster.df$member1 = c(2,10,12)
cluster.df$member2 = c(6,10,NA)
cluster.df$member3 = c(8,NA,NA)
cluster.df$mean = c(5.33,10,12)

cluster.df=data.frame(cluster.df)
cluster.df %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
cluster.df=NULL
cluster.df$clusterD= c(1,2,3)
cluster.df$mean = c(5.33,10,12)
cluster.df$clusterSize = c(3,2,1)
cluster.df$SizeXMean = c(16,20,12)
cluster.df$divide.avg.cluster.size = c(8,10,6)
cluster.df$dev2 = c(0,4,4)

cluster.df=data.frame(cluster.df)
cluster.df %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ----echo = TRUE------------------------------------------------------------------------------------------------------------------------------------------------
# Simulate a true population
  N.plots= 100 # talus slope plots
  N.mtns= 10 # mtns

# Create matrix of counts
  pop = matrix(NA, N.plots, N.mtns)

# Consider each mtn has a different mean abundance
# This forces the mtns to vary in pika abundance
  mu = seq(1,100,length.out = N.mtns)

# Loop over each mountain and draw N.plots random values from that specific mean
# log transform the mean and the exponentiate then round to make them counts 
# and to ensure values are never negative
for(j in 1:N.mtns){
  set.seed(143453543+j)
  pop[,j] = round(
                  exp(
                      rnorm(N.plots,
                            log(mu[j]),
                            0.1
                            )
                      ),
                  digits=0
                  )
}


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
fields::image.plot(matrix((data=pop), ncol=10, nrow=100),
                   axes=F, xlab="Plot #",
                   ylab="Mtn #")
axis(2, at=seq(0,1,length.out=10),lab=1:10)
axis(1, at=seq(0,1,length.out=100),lab=1:100)



## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
fields::image.plot(matrix((data=pop), ncol=10, nrow=100),
                   axes=F, xlab="Plot #",
                   ylab="Mtn #")
axis(2, at=seq(0,1,length.out=10),lab=1:10)
axis(1, at=seq(0,1,length.out=100),lab=1:100)



## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
fields::image.plot(matrix((data=pop), ncol=10, nrow=100),
                   axes=F, xlab="Plot #",
                   ylab="Mtn #")
axis(2, at=seq(0,1,length.out=10),lab=1:10)
axis(1, at=seq(0,1,length.out=100),lab=1:100)



## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
# Get true population parameters
  mu = mean(pop)
  tau = sum(pop)


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------
# SRS
  # Total number of secondary sample units for SRS
    N = N.plots*N.mtns
  # Sample size of secondary units for SRS  
    n=100

# Clusters
   # Number of primary units (plot across mtns)
    N.primary = 100

  # Plots per cluster (mtn); seconary unit
    N.secondary = 10


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------
#Number of simulated studies / replicate samples  
n.sim= 10000
  
# Create SRS function  
srs.fun = function(pop){
  
  # Get a sample of indices
  index=sample(1:(N.plots*N.mtns),size=n)
  
  # Use those indices to get our population counts in each sample unit
  y = c(pop)[index]
  
  #Total estimate
  tau.est=mean(y)*N
  
  #Standard deviation of total 
  tau.sd=sqrt(N*(N-n)*(var(y)/n))

  list(tau.est=tau.est,
       tau.sd=tau.sd)
}

# replicate!
srs.total.dist=replicate(n.sim, srs.fun(pop))



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
hist(unlist(srs.total.dist[1,]),
     main="SRS Tau Estimate",xlab="Total Population Size")
abline(v=tau,col=2,lwd=3)


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------------
cluster.fun = function(pop){
  #Get index of plots (clusters) to sample across mtn rnages and sample all within that cluster
  index=sample(1:N.primary, size = 10)
  
  y = pop[index,] # this is 10x10 (total of 100 sample units)
  y.sum = apply(y,1,sum)
  
  tau.est = N.primary*mean(y.sum)

  #Standard deviation of total 
  var.primary= var(y.sum)
  tau.sd = sqrt(N.primary*(N.primary-N.secondary)*(var.primary/N.secondary))
  
  list(tau.est=tau.est,
       tau.sd=tau.sd)
    
}

# Replicate!!!!
  cluster.total.dist=replicate(n.sim, cluster.fun(pop))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
  hist(unlist(cluster.total.dist[1,]),breaks=20,
         main="Tau Estimate")
  abline(v=tau,col=2,lwd=3)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
  hist(unlist(srs.total.dist[1,]),breaks=20,
         main="Tau Estimate",xlab="Population Estimate")
  abline(v=tau,col=2,lwd=3)
  # Add plot of Cluster
  hist(unlist(cluster.total.dist[1,]),
       col=grDevices::adjustcolor("red",alpha.f=0.1),
       breaks=20,
       add=TRUE)


## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
  hist(unlist(srs.total.dist[2,]),breaks=20,
         main="Tau Variance",xlim=c(0,4000),xlab="Population Variance")
  abline(v=tau,col=2,lwd=3)
  # Add plot of Cluster
  hist(unlist(cluster.total.dist[2,]),
       col=grDevices::adjustcolor("red",alpha.f=0.1),
       breaks=20,
       add=TRUE)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
var(c(pop))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
var(apply(pop,1,sum))

