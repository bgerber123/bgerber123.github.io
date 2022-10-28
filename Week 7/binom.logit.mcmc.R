binom.logit.mcmc <- function(y,alpha,beta,p.tune,n.mcmc){

#######################
####
####  Implements Metropolis-Hastings sampler for binomial logit, 
####  where y~Binom(N=1,p), p~Beta(alpha,beta).
####
####  Example Use:
####  
####  binom.logit.mcmc(c(1,0,1,0,1),1,1,0.2,1000) 
####  
#######################

####
####  Setup Variables 
####

p.save=rep(0,n.mcmc)
mh.p=1

####
####  Starting Values 
####

p=mean(y)
p.save[1]=p

####
####  Begin Gibbs Loop 
####
  
for(k in 2:n.mcmc){
  if(k%%100==0) cat(k," "); flush.console()

  ####
  ####  Sample p 
  ####

  #Propose a new value for p
  p.star=plogis(rnorm(1,qlogis(p),p.tune)) 

  mh1=sum(dbinom(y,size=1,p.star,log=TRUE))+dbeta(p.star,alpha,beta,log=TRUE)
  mh2=sum(dbinom(y,size=1,p,log=TRUE))+dbeta(p,alpha,beta,log=TRUE)
  mh=exp(mh1-mh2)
  
#If mh > then random number then save p.star as p  
 if(mh > runif(1)){
    p=p.star
    mh.p=mh.p+1
  }

  ####
  ####  Save Samples 
  ####

  p.save[k]=p

}

####
####  Write Output 
####
 
list(p.save=p.save,mh.p=mh.p,n.mcmc=n.mcmc)

}
