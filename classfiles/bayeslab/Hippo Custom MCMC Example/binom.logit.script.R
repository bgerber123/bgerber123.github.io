####  Source the function
  source("binom.logit.mcmc.R")


####  Prior specification
  alpha=1
  beta=1

#Plot of prior 
curve(dbeta(x,shape1=alpha,shape2=beta),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,2))


####  The Data
# Hippo survival data 

y=c(0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)

#How many iterations to run the algorithm
  n.mcmc=10000

  
#Function argument order: y,alpha,beta,p.tune,n.mcmc
####  y: vector of binomial successes of N=1
####  p: probability of success
####  alpha: prior Beta shape parameter
####  beta: prior Beta shape parameter
####  p.tune: tuning parameter (chosen to enhance mixing)
####          How much jumping around does the proposal value does

binom.logit.out=binom.logit.mcmc(y,
                                 alpha,
                                 beta,
                                 p.tune=1,
                                 n.mcmc=n.mcmc)

#What proportion of guesses were accepted?
#Want this b/w 0.2 an 0.6. Just not near 0 or 1
  binom.logit.out$mh.p/n.mcmc

####
####  View Trace Plot and M-H acceptance rate 
####

plot(binom.logit.out$p.save,type="l",ylab=expression(p),
  main=paste("Acceptance: ",binom.logit.out$mh.p/binom.logit.out$n.mcmc))

####
####  Histogram of MCMC Output
####

hist(binom.logit.out$p.save,breaks=30,xlim=c(0,1),prob=TRUE,col=8,
  main=expression(paste("Posterior for ",p)),xlab=expression(p))

####
####  Compute Posterior Summary Statistics
####

# remove the first 500 iterations as 'burn-in'

#posterior mean
mean(binom.logit.out$p.save[-(1:500)])

#posterior median
quantile(binom.logit.out$p.save[-(1:500)],probs=0.5)

# 95% Credible Intervals
quantile(binom.logit.out$p.save[-(1:500)],c(0.025,0.975))






