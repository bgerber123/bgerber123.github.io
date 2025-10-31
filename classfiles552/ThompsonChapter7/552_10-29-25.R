#Jonathan and friends

#This script is to evaluate relationships between correlation and variance.  

library(tidyverse)

N<- 1000
n<- 10

set.seed(34567)
weight<-rpois(N,5)
mu_x<-mean(weight)

#hist(weight)
  
beta1<-20
epsilon<-rnorm(N, 0, 0.1)
epsilon2<-rnorm(N, 0, 100)

  
offspring1<-0+beta1*weight+epsilon
offspring2<-0+beta1*weight+epsilon2

mu_y<-mean(offspring1)

cor(weight, offspring1)
cor(weight, offspring2)
#plot(weight, offspring1)
#plot(weight, offspring2)

estimate.r<-function(x,y){(mean(y)/mean(x))*mu_x}

save.mean<-save.r<-rep(NA,10000)

correlation<-NA
ratio_variance<-NA
srs_variance<-NA

for(j in 1:200){
  
  sigma<-j
  set.seed(2)
  epsilon2<-rnorm(N, 0, sigma)
  offspring1<-0+beta1*weight+epsilon2
  correlation[j]<-cor(weight,offspring1)
  
  for(i in 1:10000){
  
  ind<-sample(1:N, n)
  ratio_estimate<-estimate.r(x=weight[ind],y=offspring1[ind])
  non_ratio_estimate<-mean(offspring1[ind])
  save.mean[i]<-non_ratio_estimate
  save.r[i]<-ratio_estimate
  
  }
  
  ratio_variance[j]<-var(save.r)
  srs_variance[j]<-var(save.mean)
  #print(j)
}

hist(save.r)
hist(save.mean)

plot(x=correlation, y=srs_variance, type="l")
lines(x=correlation, y=ratio_variance, col="2")
