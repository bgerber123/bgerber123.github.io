
##################
# Create N-mixture replicated counts

#Model fitting

load("three.data.sets")

# Convert the Count data into under detected counts with replication - suitable for an N-mixture model. 

abundance = data[[1]]


#Again, we need to consider the number of replicates and detection probability....

# How many occasions/replicates per site?
k = 2

# Probability of detection per occasion, given occurence (z==1)
p = 0.7


#Create replcaited count data
count.rep = matrix(NA, nrow=nrow(abundance),ncol=k)

for(i in 1:nrow(abundance)){
  set.seed(543534+i)
  count.rep[i,] = rbinom(k, abundance$Counts[i], p)
}

count.rep=data.frame(count.rep=count.rep,selev1=data[[1]]$selev1)

head(count.rep)
save(count.rep,file="count.rep")
##################

# Simulate detection non-detection date from presence-absence data

# Detection non-detection data with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
y =data[[3]]

#Look at presence absences
table(y$y)

# To turn presence absences into detection non-detection,
# we need to decide on the number of replicates and detection probability
# per replicate.

# How many occasions/replicates per site?
k = 3

# Probability of detection per occasion, given occurence (z==1)
p = 0.2


#Create detection non-detection data
det.nondet = matrix(NA, nrow=length(y$y),ncol=k)

# Loop over PA data, z=1 if y$y=1.
# Flip a coin with probability p if z=1
for(i in 1:length(y$y)){
  z = y$y[i]
  if(z==0){
    det.nondet[i,] = rep(0,k)
  }else{
    set.seed(432453+i)  
    det.nondet[i,] = rbinom(k,1,p)
  }
}

det.nondet=data.frame(det.nondet=det.nondet,selev3=data[[3]]$selev3)

head(det.nondet)
save(det.nondet,file="det.nondet")
