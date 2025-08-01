#Effect of sampling bias - disproportional sampling of occupied sites
#Specific type of unrepresentiveness


#Lets say there are 100 sites
  N = 200
  prob.occ=0.5
  n.occu=prob.occ*N
  sites=1:N

# Decide on a fixed number of sites that are occupied wiht
# expect probability prob.occ
  set.seed(5454)
  occ.sites = sample(sites,n.occu,replace=FALSE)
  
  
# Crate dataframe of samples sites
  truth = data.frame(sites=sites,
                    occu = 0)
  truth$occu[occ.sites]=1


# Probability of sampling an occupied cell
  p.sample.occ=1.1/N

  p.sampling.unocc = (1-(p.sample.occ*n.occu))/(N-n.occu)

#overall probability of selecting an occupied site
  p.sample.occ*n.occu

#overall probability of selecting an occupied site
  p.sampling.unocc*(N-n.occu)

#Proportional difference of sample site being selected based on it being occupied or not
#22% higher for occupied site
    p.sample.occ/p.sampling.unocc

    truth$prob.sample=truth$occu
    truth$prob.sample[which(truth$occu==1)]=p.sample.occ
    truth$prob.sample[which(truth$occu==0)]=p.sampling.unocc


#Now simulate many samples and get proportion of occupied sites
# How many sites will we sample  
  n=60
  
#how many simulations
  n.sim=5000

probs.save=rep(NA, n.sim)

for(i in 1:n.sim){
  sample.sites=sample(truth$sites,
                     n,
                     prob = truth$prob.sample
                     )
  sampled.occu=truth$occu[sample.sites]
  probs.save[i]=mean(sampled.occu)
  
  #weighted average approach
  
  weights=1/truth[sample.sites,]$prob.sample
  sum(sampled.occu*weights/sum(weights))
  
} 

#Visualize sampling distribution of proportion sites occupied
#And line for true value
  hist(probs.save,breaks=10)
  abline(v=prob.occ,col=2,lwd=3)

# Statistical Bias
  mean(probs.save)-prob.occ


##########################################
# Now with a larger prob selection by occupied

  p.sample.occ=1.5/N
  
  p.sampling.unocc = (1-(p.sample.occ*n.occu))/(N-n.occu)
  
  #overall probability of selecting an occupied site
  p.sample.occ*n.occu
  
  #overall probability of selecting an occupied site
  p.sampling.unocc*(N-n.occu)
  
  #3x higher prob of selection of a site that is occupied
  p.sample.occ/p.sampling.unocc
  
  truth$prob.sample=truth$occu
  truth$prob.sample[which(truth$occu==1)]=p.sample.occ
  truth$prob.sample[which(truth$occu==0)]=p.sampling.unocc
  
  #create columns of ratio of weights
  truth$prob.sample.ratio=truth$prob.sample
  truth$prob.sample.ratio[which(truth$prob.sample.ratio==0.0025)]=1
  truth$prob.sample.ratio[which(truth$prob.sample.ratio==0.0075)]= 1/3
  
  head(truth)
  
#Setup for loop simulation  
  probs.save=rep(NA, n.sim)
  probs.est=rep(NA, n.sim)
  for(i in 1:n.sim){
    sample.sites=sample(truth$sites,n,replace = FALSE,
                        prob=truth$prob.sample)
    data=truth[sample.sites,]
    probs.save[i]=mean(data$occu)
    
    # options(warn=-1)
    # model=glm(occu~1,family=binomial,data=data,
    #           weights=prob.sample.ratio)
    # options(warn=0)
    # probs.est[i]=predict(model,type="response")[1]
    
    # esimtator
    probs.est[i] = sum(data$occu/data$prob.sample) /sum((1/data$prob.sample))
  }
  
  #Visualize sampling distribution of proportion sites occupied
  #And line for true value
  hist(probs.save,breaks=10,xlim=c(0,1))
  abline(v=prob.occ,col=2,lwd=3)
  
  # Statistical Bias
  mean(probs.save)-prob.occ
  
  hist(probs.est,breaks=9,xlim=c(0,1))
  abline(v=prob.occ,col=2,lwd=3)

# Statistical Bias
  mean(probs.est)-prob.occ 

######################

  #correction- Generalized unequal probabilitity estimator
  sum(data$occu/data$prob.sample) /sum((1/data$prob.sample))
  
  
  
# Consider Missing at random vs not missing at random.   
  
  