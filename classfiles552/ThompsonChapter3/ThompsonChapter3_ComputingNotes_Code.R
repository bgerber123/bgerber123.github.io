
# Chapter 3: Confidence Intervals
# 3.4. Computing Notes

# Confidence Interval Computation 
# With the caribou example,

y <- c(1, 50, 21, 98, 2, 36, 4, 29, 7, 15, 86, 10, 21, 5, 4) 

n <- length(y) 
n

N <- 286 

ybar <- mean(y) 
ybar

var_hat_y_bar <- (1- n/N) * var(y)/n 
var_hat_y_bar

tau_hat <- N * ybar 
tau_hat 

var_hat_tau_hat <- N^2 * var_hat_y_bar 
var_hat_tau_hat 

se_tau_hat <- sqrt(var_hat_tau_hat) 
se_tau_hat 

qt(0.95,df=14) 

LowerLimit <- tau_hat- qt(0.95,df=14) * se_tau_hat 
LowerLimit 
UpperLimit <- tau_hat + qt(0.95,df=14) * se_tau_hat 
UpperLimit

qt(c(0.05,0.95),df=14)

CI <- tau_hat + qt(c(0.05,0.95),df=14) * se_tau_hat 

CI

# Simulations Illustrating the Approximate Normality of a Sampling Distribution with Small n and N

# The population of 31 black cherry trees used in the computing section of Chapter 2 can now be used 
# to illustrate the finite-population central limit theorem. 
# First, the distribution of tree volumes in the population itself is illustrated with the histogram 
# of the 31 y-values. Notice the variable of interest in the population is not at all normally distributed, 
# having an asymmetric and bumpy shape. Next, simulations are run for the sampling strategy simple random 
# sampling with sample sizes 1, 2, 5, 15, 25, and 30.

y <- trees$Volume 
N <- 31 

hist(y) 
hist(y,xlim=c(10,80)) 

ybar <- numeric(0) 
b=10000 
n=1 

for (k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])} 
hist(ybar) 

n<-2 
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])} 
hist(ybar) 

n<-5 
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])} 
hist(ybar) 

n<-15 
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])}
hist(ybar) 

n<-25 
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])} 
hist(ybar) 

n<-30 
for (k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])}
hist(ybar)

#See text in book chapter

ybar <- numeric(0) 
b=10000 
n=1 

for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])} 
hist(ybar,xlim=c(10,80)) 

n<-2 
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])}
hist(ybar,xlim=c(10,80)) n<-5 

for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])} 
hist(ybar,xlim=c(10,80)) 

n<-15 
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])} 
hist(ybar,xlim=c(10,80)) 
n<-25 

for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])}
hist(ybar,xlim=c(10,80)) 

n<-30 
for(k in 1:b){s<-sample(1:N,n);ybar[k]<-mean(y[s])} 
hist(ybar,xlim=c(10,80))

# Daily Precipitation Data

# Data no longer available
