#######################################################
# Author: Brian D. Gerber
#
# Goal: To introduce the idea of the likelihood and maximum likelihood
#
# Last Updated: 6/23/2026
####################################################

#Load R packages
  library(rgl)
  library(utils)

##################################################
# The Setup

# Here is the true population of mongoose weights (lbs).
# "Population" indicates that this is the complete set of all weigth values
# that exist for however a 'population' is being defined.
# We don't usually get to see the true population, but only a single sample

weights = c(3.5948031, 2.9389464, 2.3039952, 4.4335746, 3.2380551, 5.9372273, 2.1879513, 4.6909930, 4.8180454, 2.6620834,
                2.6278748, 1.8681733, 1.7627187, 1.9559691, 3.2443797, 2.7432778, 4.1115911, 1.7454118, 2.9869501, 0.8728891,
                3.7688900, 3.1523219, 3.3175419, 3.4007622, 3.2842763, 1.1695005, 3.2339664, 2.5641397, 3.0828627, 1.4915429,
                3.7889264, 3.0619933, 1.9634965, 3.7793706, 4.0577525, 3.5826062, 2.8607450, 2.1597275, 3.4563558, 2.8209301)

#The population mean- this is truth
  mean(weights)

# How many indivdiauls are in this population?
# N=40; there are 40 animals in the population of interest
  length(weights)
  
##################################################
#Next, lets take a sample. In this sample we only get to observe 10 individual's weights
  set.seed(4534)
  sample.weights = sample(weights,10)

# We want to estimate the population mean from our sample. Lets consider possible values that the mean could take.
# For now, we will ignore estimating the sample variance by fixing it to 1. 

  possible.mu = seq(0.01,10,0.01)  #change 0.1 to 0.001
  sigma1 = 1

# That's a lot of possible mu, but still not all the possible mu, right?
  possible.mu

# Let's do a brute force search of lots possible values of the population mean where we assume
# y ~ Normal(mu, sigma=1)

# We will calculate the likelihood (product of the probability density for a given value of mu). 
# By searching over different values of mu, we want to evaluate the value of mu that maximizes the probability
# of observing the data we observed. Or, more simply, the likelihood of the data.

# Create an object to store the likelihood value for each possible.mu
  save.likelihood = rep(0, length(possible.mu))

# Next, loop over possible.mu values and calculate the likelihood of the data
  for(i in 1:length(possible.mu)){
      save.likelihood[i] = prod(
                                dnorm(sample.weights, possible.mu[i], sigma1)
                                )
    }

#Now, lets visualize the likelihood for each value of mu by mu
  #X11()
  par(mfrow=c(2,1))
  plot(possible.mu,save.likelihood, main="N =10")

# Find the index of the the maximum likelihood value
  which.max(save.likelihood)

# Extract the actual maximum likelihood value and plot it
  mle=possible.mu[which.max(save.likelihood)]
  abline(v=mle,col=2,lwd=3)

#this is our best estimate of mu - the population mean of mongoose weights
  mle

#########################################################
#########################################################
# Now, lets get fancy and use an optimization algorithm, which
# will be more accurate than our brute force search
  
# Specifcally, we will use the optimize function in stats package. This function
# will aim to minimize any function that you provide it

# First, we need to specify our likelihood function. Note the "-1" which will 
# mean we will find the maximum likeihood value by actually finding the minimum
# likelihoof value

normal.likelihood = function(x){
                                (-1)*prod(dnorm(sample.weights,x,1))
                                }
optimize(f = normal.likelihood,
         interval = c(0,10)
         )

#The best estimate is what? Is it similar to our brute force search that is the mle object?


# Let's help out the algorithim. Taking products can cause issues when values get really small. Computers don't like really
# small or really large numbers. So, let's change this to summing the log of the probability densities

normal.likelihood = function(x){
                                (-1)*sum(dnorm(sample.weights,x,1,log=TRUE))
                                }

optimize(f = normal.likelihood,interval=c(0,10))

#Note how our objective value changed, but we get the same mle

# Let's compare this with using the glm function
  glm.out = glm(sample.weights~1,family=gaussian)
  glm.out$coefficients

# Is the same value as in the brute force and our fancy optimization?

# Getting simple - What if we just took the sample mean....
  mean(sample.weights)

# Why bother with finding the mle if we can just take the mean of the sample? 
  
# Many reasons, but first, consider that the principal of maximum likelihood provides justification
# of why the sample mean (under the normal distribution) is a really good thing to do.

#################################################
# Plot the true mean next to the mle and likelihood profile
  abline(v=mean(weights),col=3,lwd=3)

# Is our estimate (mle) biased?
# Consider the difference between the mle and truth...
mle
mean(weights)

# This is not statisical bias. What is it called then? It is not a bad thing. 
# It is an expected property of a sample.

#####################################################
######################################################
# Next, lets investigate how the likelihood changes when the data changes.

# Lets sample 25 (out of 30) mongoose randomly and brute force the likelihood profile
  set.seed(654654)
  sample.weights2=sample(weights,25)

#store values in this object:
  save.likelihood2=rep(0, length(possible.mu))

#loop over possible.mu
  for(i in 1:length(possible.mu)){
                save.likelihood2[i]=prod(
                                         dnorm(
                                                sample.weights2, possible.mu[i], sigma1
                                                )
                                         )
}

#plot the likelihood profile
  plot(possible.mu,save.likelihood2, main="N = 25")

#Find the mle
mle2 = possible.mu[which.max(save.likelihood2)]
abline(v=mle2,col=2,lwd=3)

# Plot the truth
abline(v=mean(weights),col=3,lwd=3)


#What about variation - how spread out is the likelihood and does it change by sample size - why?
sd(save.likelihood) #  N=10
sd(save.likelihood2)  #N=25

# The likelihood profile is more precise with more data!
# The width (variation) of the likelihood profile holds information about the uncertainty of our parameters.
# This is where we are getting measures of standard error

#####################################################
######################################################
# Lets no longer ignore sigma, but jointly estimate mu and sigma
# This requires us to find the joint likelihood of mu and sigma simultanesouly

# Consider possible value of mu and sigma
  possible.mu=seq(0.01,4,0.05)  
  possible.sigma=seq(0.5,4,0.05)

# Find all possible combinations of mu and sigma
 possible.combs=expand.grid(possible.mu,possible.sigma)

 head(possible.combs)

# New storage object
 save.likelihood=rep(0, dim(possible.combs)[1])

#loop over each combination of both possible mu and sigma, calculating the joint likelihood of the data
  for(i in 1:length(save.likelihood)){
            save.likelihood[i]=sum(dnorm(sample.weights, 
                                         possible.combs[i,1], 
                                         possible.combs[i,2],
                                         log=TRUE)
                                   )
}

# Maximum likelihood value
  save.likelihood[which.max(save.likelihood)]

# mle for mu and sigma
  possible.combs[which.max(save.likelihood),]


# Let's plot the 3d likelihood surface
# First we need cool colors- create a vector or rainnow colors
  cols <- rainbow(10000)[as.numeric(cut(save.likelihood,breaks = 10000))]

# Now find the values that are > -20, which is near the mle and make them black to stand out visually
  cols[which(save.likelihood>-20)]="black"

# What is the index for the mle and then turn it white
  which.max(save.likelihood)
  cols[373]="white"

#Create the 3D plot
rgl::plot3d(possible.combs[,1],possible.combs[,2],save.likelihood,xlab="mu",ylab="sigma",col=cols,size=10)



#######################################################
# Lets get fancy again with a 2-parameter optimization

# Define the likelihood function that we want to minimize
normal.likelihood=function(x){(-1)*sum(
                                      dnorm(sample.weights,
                                            x[1],
                                            x[2],
                                            log=TRUE)
                                      )
                              }

#use the function optim with 0.5 as starting values for mu and sigma
max.optim = optim(c(0.5,0.5),
                  normal.likelihood,
                  method="Nelder-Mead"
                  )
#here are the mle's
max.optim$par

#########################################################
#Now for another MLE example, but this time with survival data (1= survive, 0 = dead).

# Imagine a GPS tracking study that you record whether an animal survived the winter or died.

# 0 and 1 data don't make sense to use the Normal distribution. We will use the binomial likelihood now

# logit and logit-inverse (expit) function. The logit function maps probabilities to the real number line and then the
# expit function maps the real number line estimates back to probabilities.

# Load functions
logit=function(x){log(x)/(1-x)}
expit=function(x){exp(x)/(exp(x)+1)}

#These are the same as the function, qlogis and plogis in the stats package

survival.data=c(1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 
                1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 
                0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0)

#consider all the possible survival probabilities
  possible.p = seq(0.01,1,0.01)

#create a storage object and loop over all possible.p with the survival data and find the parameter that is most likely, given the data
  save.likelihood3=rep(0, length(possible.p))
  
  for(i in 1:length(possible.p)){
          save.likelihood3[i]=sum(dbinom(survival.data,
                                         1, 
                                         possible.p[i],
                                         log=TRUE)
                                  )
      }

#plot the likelihood profile
  plot(possible.p,save.likelihood3)

#What is the mle?
  possible.p[which.max(save.likelihood3)]


#Being fancy - optimization for the mle (optim minimizes the function)
  binomial.likelihood=function(x){
                  (-1)*sum(dbinom(survival.data,1,x,log=TRUE))
                  }

  optimize(f = binomial.likelihood,interval=c(0,1))

#Do you get the same or similar answer?

#Compare to fitting a logistic regression model with the glm function
  glm.logisitic=glm(survival.data~1, family="binomial")
  summary(glm.logisitic)

#Where is the predicted probability of survival (y==1)? We need to backtransform out estimate
#take the inverse-logit of the beta coefficient
expit(glm.logisitic$coefficients[1])


#Or we can ask R to do it for us. We only need the first prediction, as they are all the same under this model
  predict(glm.logisitic,type = "response")[1]

