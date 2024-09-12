#######################################################
#Author: Brian Gerber
#
#Goal: To introduce the idea of the likelihood and maximum likelihood
####################################################

#load libraries
library(rgl)
library(utils)

#The TRUE poplation of mongoose weights in lbs
#We don't normally get to see the true population...only a sample
weights=c(3.5948031, 2.9389464, 2.3039952, 4.4335746, 3.2380551, 5.9372273, 2.1879513, 4.6909930, 4.8180454, 2.6620834,
                2.6278748, 1.8681733, 1.7627187, 1.9559691, 3.2443797, 2.7432778, 4.1115911, 1.7454118, 2.9869501, 0.8728891,
                3.7688900, 3.1523219, 3.3175419, 3.4007622, 3.2842763, 1.1695005, 3.2339664, 2.5641397, 3.0828627, 1.4915429,
                3.7889264, 3.0619933, 1.9634965, 3.7793706, 4.0577525, 3.5826062, 2.8607450, 2.1597275, 3.4563558, 2.8209301)

#The population mean- this is truth
mean(weights)

#N=40; there are 40 animals in the population of interest
length(weights)


##################################################
#Lets take a sample- we get to observe 10 individuals and weigh them
set.seed(4534)
sample.weights=sample(weights,10)

#We want to estimate the population mean from our sample. Lets consider possible values that the mean could take.
#For now, we will ignore estimating the sample variance by fixing it to 1. 
possible.mu=seq(0.01,10,0.01)  #change 0.1 to 0.001
sigma1=1

#That's a lot of possible mu, but still not all the possible mu, right?
possible.mu

#Do a brute force search of lots possible values of the population mean where we assume
#y ~ Normal(mu, sigma=1)

#We will calculate the likelihood (product of the probabliity density for a given value of mu). 
#By searching over differnt values of mu, we want to value the value of mu that maximizes the probability
#of observing the data we observed. Or, more correctly the liklhood of the data.

#creat object to store likelihood for each possible.mu
save.likelihood=rep(0, length(possible.mu))

#loop over possible.mu and calculate likelihood of the data
for(i in 1:length(possible.mu)){
save.likelihood[i]=prod(dnorm(sample.weights, possible.mu[i], sigma1))
}

#plot the likelihood for each value of mu by mu
#X11()
par(mfrow=c(2,1))
plot(possible.mu,save.likelihood)

#find the index of the the maximum likelihood value
which.max(save.likelihood)

#extract the actual maximum likelihood value and plot it
mle=possible.mu[which.max(save.likelihood)]
abline(v=mle,col=2,lwd=3)

#this is our best estimate of mu - the population mean of mongoose weights
mle


#Getting fancy - optimization for the mle (optim minimizes the function)
#Instead of doing a brute force search, lets use an algorithm that minimizes 
#our function and searches for the parameter that does this. 
normal.likelihood=function(x){(-1)*prod(dnorm(sample.weights,x,1))}
optimize(f = normal.likelihood,interval=c(0,10))

#The best estimate is what? Is si simiar to our brute force search that is the mle object?


#Getting fancy - taking products can cause issues. Let's change this to summing the log of the probability densities
normal.likelihood=function(x){(-1)*sum(dnorm(sample.weights,x,1,log=TRUE))}
optimize(f = normal.likelihood,interval=c(0,10))

#or lets use glm
glm.out=glm(sample.weights~1,family=gaussian)
glm.out$coefficients

#Is the same value as in the brute force and our fancy optimization?

#Getting simple - What if we just took the sample mean....
mean(sample.weights)

#Why bother with mle? 
#Many reasons, but first, consider that the principal of maximum likelihood provides justification
#of why the sample mean (under the normal distribution) is a really good thing to do.


#Plot the true mean next to the mle and likelihood profile
abline(v=mean(weights),col=3,lwd=3)

#Is our estimate (mle) biased?
#Consider the difference between the mle and truth...
mle
mean(weights)

#Are you sure?

#####################################################
######################################################
#How does the likelihood change when the data changes.
#Lets sample 25 animamls randomly and brute force the likelihood profile
set.seed(654654)
sample.weights2=sample(weights,25)

#store values in this object:
save.likelihood2=rep(0, length(possible.mu))

#loop over possible.mu
for(i in 1:length(possible.mu)){
save.likelihood2[i]=prod(dnorm(sample.weights2, possible.mu[i], sigma1))
}

#plot the likelihood profile
plot(possible.mu,save.likelihood2)

#Find the mle
mle2=possible.mu[which.max(save.likelihood2)]
abline(v=mle2,col=2,lwd=3)

#Plot the truth
abline(v=mean(weights),col=3,lwd=3)


#variation - how spread out is the likelihood and does it change by sample size - why?
sd(save.likelihood) #N=10
sd(save.likelihood2)  #N=25

#The likelihood profile is more precise with more data!
#The width (variation) of the likelihood profile holds information about the uncertainty of our parameters.
#This is where we are getting measures of standard error

#####################################################
######################################################
#Lets no longer ignore sigma, but jointly estimate mu and sigma
#brute force

#consider possible value of mu and sigma
possible.mu=seq(0.01,4,0.05)  
possible.sigma=seq(0.5,4,0.05)

#find all possible combinations of mu and sigma
possible.combs=expand.grid(possible.mu,possible.sigma)

head(possible.combs)

#new storage object
save.likelihood=rep(0, dim(possible.combs)[1])

#loop over each combination of both possible mu and sigma, calculating the joint likelihood of the data
for(i in 1:length(save.likelihood)){
save.likelihood[i]=sum(dnorm(sample.weights, possible.combs[i,1], possible.combs[i,2],log=TRUE))
}

#maximum likelihood value
save.likelihood[which.max(save.likelihood)]

#mle for mu and sigma
possible.combs[which.max(save.likelihood),]


#Let's plot the 3d likelihood surface
#First we need cool colors- create a vector or rainnow colors
cols <- rainbow(10000)[as.numeric(cut(save.likelihood,breaks = 10000))]

#now find the values that are > -20, which is near the mle and make them black to stand out visually
cols[which(save.likelihood>-20)]="black"

#What is the index for the mle and then turn it white
which.max(save.likelihood)
cols[854]="white"

#Create the 3D plot
rgl::plot3d(possible.combs[,1],possible.combs[,2],save.likelihood,xlab="mu",ylab="sigma",col=cols,size=10)



#######################################################
#Brute force is lame, lets get fancy again with TWO-parameter optimization
#Define the likelihood function that we want to minimize
normal.likelihood=function(x){(-1)*sum(dnorm(sample.weights,x[1],x[2],log=TRUE))}

#use the function optim with 0.5 as starting values for mu and sigma
max.optim=optim(c(0.5,0.5),normal.likelihood,method="Nelder-Mead")
#here are the mle's
max.optim$par



#########################################################
#Another MLE example, but this time with survival data (1= survive, 0 = dead).
#Imagine a GPS tracking study that you record whether an animal survived the winter or died.

#0 and 1 data don't make sense to use the Normal distribution. We will use the binomial likelihood now

#logit and logit-inverse (expit) function. The logistic function maps probabilities to the Real Number line and then the
#expit function maps the real number line estimates back to probabilities.
#Load functions
logit=function(x){log(x)/(1-x)}
expit=function(x){exp(x)/(exp(x)+1)}


survival.data=c(1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 
                1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 
                0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0)

#consider all the possible survival probabilities
possible.p=seq(0.01,1,0.01)

#create a storage object and loop over all possible.p with the survival data and find the parameter that is most likely, given the data
save.likelihood3=rep(0, length(possible.p))
for(i in 1:length(possible.p)){
save.likelihood3[i]=sum(dbinom(survival.data,1, possible.p[i],log=TRUE))
}

#plot the likelihood profile
plot(possible.p,save.likelihood3)

#What is the mle?
possible.p[which.max(save.likelihood3)]



#Being fancy - optimization for the mle (optim minimizes the function)
binomial.likelihood=function(x){(-1)*sum(dbinom(survival.data,1,x,log=TRUE))}
optimize(f = binomial.likelihood,interval=c(0,1))

#Do you get the same or similar answer?

#Compare to fitting a logistic regression model with the glm function
glm.logisitic=glm(survival.data~1, family="binomial")
summary(glm.logisitic)

#Where is the predicted probability of survival (y==1)? We need to backtransform out estimate
#take the inverse-logit of the beta coefficient
expit(0.7538)


#Or we can ask R to do it for us. We only need the first prediction, as they are all the same under this model
predict(glm.logisitic,type = "response")[1]

