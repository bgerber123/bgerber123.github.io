######################
#
# Objective: We will convert the presence-absence data into detection non-detection data.
#           You will then need to create a JAGS model to fit these data. Once this is figured out   
#           incorprate this new model into an integrated model along with the two count datasets
#


load("three.data.sets")

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

head(det.nondet)
save(det.nondet,file="det.nondet")

#######################
#######################
# Assignment

# Step 1

# Fit these data 'det.nondet' using a Bayesian occupancy model in JAGS, but use the cloglog link instead of logit. 
# Fit the same linear model on psi as done using the original data 'y$y', such that cloglog(psi[k]) = beta0 + beta1*selev3[k]. Incorporate
# detection probability, but do not put any covariates on this parameter (we simulated detection as constant with probability p of 0.2)
# Evaluate convergence of the posteriors and then visualize the posteriors and investigate if they are similar to when there was no issue of detection probability 
# (i.e, when using presence absence data).

# Step 2

# Adapt the integrated model we developed to fit these data (det.nondet), instead of the original data y$y ,with the two sets of count data.
# Evaluate whether the posteriors have converged. Visualize the posteriors of the intercept and slope.

