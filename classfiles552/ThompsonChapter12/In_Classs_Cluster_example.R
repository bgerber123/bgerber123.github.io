#######################
#
# Compare SRS to Cluster to estimate total population size
#
########################

# Load package - not necessary for exercise

library(fields)

# We are interested in the total population size of fish

# In our system, there are 10 streams that we can divide each into 100 reaches/segments

# Thus, in total there are 10 x 100 sample units in a SRS sense.

# We expect the most deviation to occur across streams, rather than in them. Why is that? I don't know.
# Ask a fishy colleague. But that's our reality!

# We are going to cluster across streams - aggregate segments across streams.


# Simulate a true population
  x.max= 100 # stream reach
  y.max= 10 # streams

# Create matrix
  pop = matrix(0,x.max,y.max)


# Consider each stream has a different mean - thus focing the streams to vary in fish counts
  mu = seq(1,100,length.out=y.max)

# Loop over each stream and draw x.max random values from that specific mean
# log transform the mean and the exponentiate and round to make them counts 
# and to ensure values are never negative
for(j in 1:y.max){
  set.seed(143453543+j)
  pop[,j] = round(exp(rnorm(x.max,log(mu[j]),0.1)),digits=0)
}

# look at our population
pop
fields::image.plot(matrix((data=pop), ncol=100, nrow=10))

# Get true population parameters
  mu = mean(pop)
  mu
  tau = sum(pop)
  tau
  
  
###################################  
# Setup simulation  
  
# Total number of secondary sample units for SRS
  N = x.max*y.max
# Sample size of secondary units for SRS  
  n=100
  
# Total number of clusters  
  N.cluster = x.max
# Sample size of primary units  
  n.cluster = 10

# The same number of total units are sampled that necessitate fish counts - 100  
    
  
  
###################################
# SRS
  
#Number of simulated studies / replicate samples  
n.sim= 10000
  
# Create SRS function  
srs.fun = function(pop){
  
  # Get a sample of indices
  index=sample(1:(x.max*y.max),size=n)
  
  # Use those indices to get our population counts in each sample unit
  y = c(pop)[index]
  
  #Total estimate
  tau.est=mean(y)*N
  
  #Standard deviation of total 
  tau.sd=sqrt(N*(N-n)*(var(y)/n))

  list(tau.est=tau.est,
       tau.sd=tau.sd)
}

#replicate!!!!!
  srs.total.dist=replicate(n.sim, srs.fun(pop))
  
  rownames(srs.total.dist)  
  
hist(unlist(srs.total.dist[1,]),
     main="SRS Tau Estimate")
abline(v=tau,col=2,lwd=3)

hist(unlist(srs.total.dist[2,]),
     main="SRS Tau SD Estimate")
abline(v=sd(y)*N,col=2,lwd=3)
#check for bias on SD of

###################################

# Cluster
cluster.fun = function(pop){
  index=sample(1:x.max,size=n.cluster)
  
  y = pop[index,]

  y.sum = apply(y,1,sum)
  
  
  tau.est=N.cluster*mean(y.sum)

  #Standard deviation of total 
  var.primary= var(y.sum)
  tau.sd = sqrt(N.cluster*(N.cluster-n.cluster)*(var.primary/n.cluster))
  
  list(tau.est=tau.est,
       tau.sd=tau.sd)
    
}

# Replicate!!!!
  cluster.total.dist=replicate(n.sim, cluster.fun(pop))

# tau estimate
  
  # Plot SRS  
  hist(unlist(srs.total.dist[1,]),breaks=20,
         main="Tau Estimate")
  abline(v=tau,col=2,lwd=3)
  # Add plot of Cluster
  hist(unlist(cluster.total.dist[1,]),
       col=grDevices::adjustcolor("red",alpha.f=0.1),
       breaks=20,
       add=TRUE)

  
# SD of tau estimate
  
  # Plot SRS  
  hist(unlist(srs.total.dist[2,]),xlim=c(0,4000),breaks=20,
       main="Tau SD Estimate")
  # Add plot of Cluster
  hist(unlist(cluster.total.dist[2,]),
       col=grDevices::adjustcolor("red",alpha.f=0.1),
       breaks=20,
       add=TRUE)
  
  
  
  
  
#clusters are mostly similar
apply(pop,1,sum)
sd(apply(pop,1,sum))

#compare to population variation
sd(pop)

