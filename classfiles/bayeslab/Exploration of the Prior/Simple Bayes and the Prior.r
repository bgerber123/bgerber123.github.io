######################################
######################################
#Author: Brian Gerber
#Date Modified: 10/14/2018

#Objective: To investigate the influence of the Prior in a Bayesian study.

######################################
######################################

#Here is the population of interest- weights of 40 animals

weights=c(3.5948031, 2.9389464, 2.3039952, 4.4335746, 3.2380551, 5.9372273, 2.1879513, 4.6909930, 4.8180454, 2.6620834,
                2.6278748, 1.8681733, 1.7627187, 1.9559691, 3.2443797, 2.7432778, 4.1115911, 1.7454118, 2.9869501, 0.8728891,
                3.7688900, 3.1523219, 3.3175419, 3.4007622, 3.2842763, 1.1695005, 3.2339664, 2.5641397, 3.0828627, 1.4915429,
                3.7889264, 3.0619933, 1.9634965, 3.7793706, 4.0577525, 3.5826062, 2.8607450, 2.1597275, 3.4563558, 2.8209301)

##################################################
#Lets take a sample- we get to observe 10 individuals and weigh them
#The set.seed make sure the random sample is the same every time the code is run
set.seed(4534); sample.weights=sample(weights,5)

#First, lets get our likelihood profile for the mean for this set of data.
#We will again keep it simple and assume the standard deviation is known 
#and doesn't need to be estimated. Typically we estimate it

possible.mu=seq(0.01,10,0.01)  
sigma1=1

#Do a search of lots of possible values of the population mean where we assume
#y ~ Normal(mu, sigma=1)

#We will calculate the likelihood (product of the probability density for a given value of mu). 
#By searching over different values of mu, we want to find the value of mu (mean) that maximizes the probability
#of observing the data we observed. Or, the likelihood of the data.
save.likelihood=rep(0, length(possible.mu))
for(i in 1:length(possible.mu)){
save.likelihood[i]=prod(dnorm(sample.weights, possible.mu[i], sigma1))
}

par(mfrow=c(1,1))
#This is the likelihood profile
plot(possible.mu,save.likelihood,type="l",lwd=3,xlim=c(0,10))
#Add the maximum likelihood value
mle=mean(sample.weights)
abline(v=mle,col=3,lwd=3)
title(bquote("MLE = "~.(round(mle,digits=3))))
##############################################################
#Lets now move to the Bayesian world and specify prior information about our unknonwn parameter
#that we want to estimate.

#First, lets consider specifiying a diffuse prior- where the probability density is spread 
#over a very large region of parameter space - meaning lots of possible paramter values have 
#a small amount of information

#Normal prior hyperparameters
prior_mu = 0
prior_sigma2=1000000

#Plot the prior over two sets of ranges
X11()
par(mfrow=c(2,1))
plot(density(rnorm(100000,prior_mu,sqrt(prior_sigma2))),lwd=3,xlim=c(-100,100),main="Prior Probability Density \n over the range of -100 to 100")
plot(density(rnorm(100000,prior_mu,sqrt(prior_sigma2))),lwd=3,xlim=c(-1000,1000),main="Prior Probability Density \n over the range of -100000 to 100000")

##############################################
#Load the posterior distribution function for Normal Likelihood and Normal Prior. The Normal distribution 
#prior is conjugate with the Normal distribution likelihood, which means we can analytically state
#the posterior distribution of the unknown Mean (mu) that is being estimated
#For more info, see https://en.wikipedia.org/wiki/Conjugate_prior

#prior_mu and prior_sigma2 are called hyperparameters and are the first and second moments of the Normal distribution.
#By knowing the mean and variance, you can specify the whole Normal Probability Density Function. More generally,
#moments can describe any probability distribution, however, the first and second (mean and variance), are usually
#the focus. The use of thinking about moments is that there is a method, "called Method of Moments", which uses 
#the law of large numbers to estimate moments using data without necessarily assuming a probability distribution.

PD_mu=function(prior_mu, prior_sigma2, data,data_variance){
  mu_pd=(1/((1/prior_sigma2)+(length(data)/data_variance)))*((prior_mu/prior_sigma2)+(sum(data)/data_variance))
  sigma2_pd=1/((1/prior_sigma2)+(length(data)/data_variance))
  return(list(mu_pd=mu_pd,sigma2_pd=sigma2_pd))
}


#Plug in the values in the function for 1) the hyperparmeter (prior) mean ("prior_mu"), 2) the hyperparmeter (prior) 
#variance ("prior_sigma2"), #3) the data (which we assume comes from a Normal distribution; "data"), and 
#4) the known variance of the data ("data_variance"). #What is estimated are the mean and variance of the 
#posterior distibution of mu (the mean weight of the animal). From the mean and variance of the posterior, we can then
#sample draws from the posterior distrubtion so to visualize and use the posteriod distribution.

postdist=PD_mu(prior_mu=prior_mu, prior_sigma2=prior_sigma2, data=sample.weights,data_variance=1)

#First, lets take a look at the mean of the posterior distribution for mu and compare it to the mle
mean(postdist$mu_pd)
mle

#Sample values from the posterior probability distribution
PD_Mu=rnorm(100000,mean = postdist$mu_pd,sd = sqrt(postdist$sigma2_pd))
par(mfrow=c(1,1))
plot(density(PD_Mu),lwd=3,xlim=c(0,10), ylab="Probability Density", main="Posterior Distribution of the Mean, \n prior, and Likelihood Profile")
abline(v=mean(postdist$mu_pd),col=1,lty=2,lwd=3)

#plot the prior density on this figure (green line)
curve(dnorm(x, prior_mu,prior_sigma2),col=3,lwd=3,add=TRUE)

#Remember that the prior density is over a much larger region, so if it happened to be flat over this 
#this parameter space (0,10), but large in another parameter space (e.g., -100 to -10), we would 
#still see that the prior looked flat. But, we know from plotting the prior above that the probabilty 
#density is pretty equivalent over a very large number of possible parameter values.

#Now add the likelihood profile to the posterior distribution as a red dashed line
#The likelihood and posterior are not on the same scale, so we need two different y-axes.
par(new = TRUE)
plot(possible.mu,save.likelihood, type = "l", lwd=3,col=2,lty=2,axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(save.likelihood)),col.axis="red")
mtext("Likelihood", col=2,side=4, line=3)

#Add the mle line as well a red dotted line
abline(v=mle,col=2,lty=3,lwd=3)

legend("topright",lwd=3,col=c(1,2,3),legend=c("Posterior","Likelihood","Prior"))

#Getting kind of crowded on the graph!
#############################################################
#############################################################
###########################
#Lets specify a new prior that suggest the weight of our individual has a mean of 5, but we are little uncertain
#about this (before the study), such that there is some information that the animal could range of 0.1 to 10.
#Normal prior hyperparameters
prior_mu = 5
prior_sigma2=2
#look at the prior
par(mfrow=c(1,1))
plot(density(rnorm(100000,prior_mu,sqrt(prior_sigma2))),lwd=3,xlim=c(-10,10),main="Prior Probability Density \n over the range of -10 to 10")

#Get the parameters of the posterior distribution of the mean (mu)
postdist=PD_mu(prior_mu=prior_mu, prior_sigma2=prior_sigma2, data=sample.weights,data_variance=1)

#First, lets take a look at the mean of the posterior distribution for mu and compare it to the mle
mean(postdist$mu_pd)
mle

#What's different and why?

#Sample values from the posterior probability distribution
PD_Mu=rnorm(100000,mean = postdist$mu_pd,sd = sqrt(postdist$sigma2_pd))
par(mfrow=c(1,1))
plot(density(PD_Mu),lwd=3,xlim=c(0,10), ylab="Probability Density", main="Posterior Distribution of the Mean, \n prior, and Likelihood Profile")
abline(v=mean(postdist$mu_pd),col=1,lty=2,lwd=3)

#plot the prior density on this figure (green line)
curve(dnorm(x, prior_mu,prior_sigma2),col=3,lwd=3,add=TRUE)

#Now add the likelihood profile to the posterior distribution as a red dashed line
#The likelihood and posterior are not on the same scale, so we need two different y-axes.
par(new = TRUE)
plot(possible.mu,save.likelihood, type = "l", lwd=3,col=2,lty=2,axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(save.likelihood)),col.axis="red")
mtext("Likelihood", col=2,side=4, line=3)

#Add the mle line as well a red dotted line
abline(v=mle,col=2,lty=3,lwd=3)

legend("topright",lwd=3,col=c(1,2,3),legend=c("Posterior","Likelihood","Prior"))


#What is going on here? What did the prior information do?
#Be able to explain how the likelihood, posterior, and prior are related.
#Also make sure you understand the difference between a probability density and a likelihood.
#What does the height of the prior information tell you about the quantity of information being provided?

################################################
################################################
#Let's take a more extreme case where we are saying there is a lot of information about the mean weight of 
#this species and that the mean should be really close to 5.

prior_mu = 5
prior_sigma2=0.01
#look at the prior
par(mfrow=c(1,1))
plot(density(rnorm(100000,prior_mu,sqrt(prior_sigma2))),lwd=3,xlim=c(0,10),main="Prior Probability Density \n over the range of 0 to 10")

#Get the parameters of the posterior distribution of the mean (mu)
postdist=PD_mu(prior_mu=prior_mu, prior_sigma2=prior_sigma2, data=sample.weights,data_variance=1)

#First, lets take a look at the mean of the posterior distribution for mu and compare it to the mle
mean(postdist$mu_pd)
mle

#Is the difference b/w the mle and posterior mean larger or smaller than before?

#Sample values from the posterior probability distribution
PD_Mu=rnorm(100000,mean = postdist$mu_pd,sd = sqrt(postdist$sigma2_pd))
par(mfrow=c(1,1))
plot(density(PD_Mu),lwd=3,xlim=c(0,10), ylab="Probability Density", main="Posterior Distribution of the Mean, \n prior, and Likelihood Profile")
abline(v=mean(postdist$mu_pd),col=1,lty=2,lwd=3)

#plot the prior density on this figure (green line)
curve(dnorm(x, prior_mu,prior_sigma2),col=3,lwd=3,add=TRUE)


#Now add the likelihood profile to the posterior distribution as a red dashed line
#The likelihood and posterior are not on the same scale, so we need two different y-axes.
par(new = TRUE)
plot(possible.mu,save.likelihood, type = "l", lwd=3,col=2,lty=2,axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(save.likelihood)),col.axis="red")
mtext("Likelihood", col=2,side=4, line=3)

#Add the mle line as well a red dotted line
abline(v=mle,col=2,lty=3,lwd=3)

legend("topright",lwd=3,col=c(1,2,3),legend=c("Posterior","Likelihood","Prior"))


#How much information did our data contribute to estimating the mean weight?
#How much information did our prior contribute to estimating the mean weight?

#######################################################
#######################################################
#Let's add a new dimension to this information game. What if our sample size was larger?
#How does that infleunce the posterior distirbution and the importance of the prior?

#Lets choose a prior that is quite different than the truth.
prior_mu = 14
prior_sigma2=0.75


#Lets consider sample sizes of 5, 10, 20, and 40
#Plug in different data sets of different size. 
set.seed(14546353); sample.weights10=sample(weights,10); sample.weights20=sample(weights,20);  sample.weights40=weights

#Plugin each data set to our function to get the parameters of the posterior distribution
postdist_N5=PD_mu(prior_mu=prior_mu, prior_sigma2=prior_sigma2, data=sample.weights,data_variance=1)
postdist_N10=PD_mu(prior_mu=prior_mu, prior_sigma2=prior_sigma2, data=sample.weights10,data_variance=1)
postdist_N20=PD_mu(prior_mu=prior_mu, prior_sigma2=prior_sigma2, data=sample.weights20,data_variance=1)
postdist_N40=PD_mu(prior_mu=prior_mu, prior_sigma2=prior_sigma2, data=sample.weights40,data_variance=1)

#Next we need to get the likelihood profile for each set of data because our data is changing.
possible.mu=seq(0.01,20,0.01) #FYI- make sure the likelihood has the same axis as that is plotted

save.likelihood=matrix(0,nrow=length(possible.mu),ncol=4)
for(i in 1:length(possible.mu)){
  save.likelihood[i,1]=prod(dnorm(sample.weights, possible.mu[i], sigma1))
  save.likelihood[i,2]=prod(dnorm(sample.weights10, possible.mu[i], sigma1))
  save.likelihood[i,3]=prod(dnorm(sample.weights20, possible.mu[i], sigma1))
  save.likelihood[i,4]=prod(dnorm(sample.weights40, possible.mu[i], sigma1))
}


#Plot the likelihoods. Because the data is different for each, they are not direclty comparable
par(mfrow=c(2,2))
plot(possible.mu,save.likelihood[,1],xlim=c(0,10),type="l",lwd=3,lty=2,main="Likelihood")
plot(possible.mu,save.likelihood[,2],xlim=c(0,10),type="l",lwd=3,col=2,lty=2,main="Likelihood")
plot(possible.mu,save.likelihood[,3],xlim=c(0,10),type="l",lwd=3,col=3,lty=2,main="Likelihood")
plot(possible.mu,save.likelihood[,4],xlim=c(0,10),type="l",lwd=3,col=4,lty=2,main="Likelihood")


#However, we can compare a few things. For example, lets look at the variation in the likelihood by sample size.
#What happens to variation in the likelihood as the sample size increases?
cbind(c(5,10,20,40),apply(save.likelihood,2,sd))

#Do these results make sense?

#Get the MLE's
indices.mle=apply(save.likelihood,2,which.max)
mle.N=possible.mu[indices.mle]

#Get the postrior means, compare to mle's
cbind(mle.N,c(postdist_N5$mu_pd,postdist_N10$mu_pd,postdist_N20$mu_pd,postdist_N40$mu_pd))


##########################################
#Plot the likelihood, prior, and posterior for each data set
PD_Mu=cbind(rnorm(1000000,mean = postdist_N5$mu_pd,sd = sqrt(postdist_N5$sigma2_pd)),
            rnorm(1000000,mean = postdist_N10$mu_pd,sd = sqrt(postdist_N10$sigma2_pd)),
            rnorm(1000000,mean = postdist_N20$mu_pd,sd = sqrt(postdist_N20$sigma2_pd)),
            rnorm(1000000,mean = postdist_N40$mu_pd,sd = sqrt(postdist_N40$sigma2_pd)))
sample.size=c(5,10,20,40)



X11()
par(mfrow=c(2,2))
for(i in 1:4){
plot(density(PD_Mu[,i]),lwd=3,xlim=c(0,20), ylab="Probability Density", main="Posterior Distribution of the Mean, \n prior, and Likelihood Profile")
abline(v=mean(PD_Mu[,i]),col=1,lty=2,lwd=3)
curve(dnorm(x, prior_mu,prior_sigma2),col=3,lwd=3,add=TRUE)
par(new = TRUE)
plot(possible.mu,save.likelihood[,i], type = "l", lwd=3,col=2,lty=2,axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(save.likelihood[,i])),col.axis="red")
mtext("Likelihood", col=2,side=4, line=3)
abline(v=mle.N[i],col=2,lty=3,lwd=3)
text(17,max(save.likelihood[,i])*0.5,bquote("N = "~.(sample.size[i])),cex=2)
legend("topright",lwd=3,col=c(1,2,3),legend=c("Posterior","Likelihood","Prior"))
}

