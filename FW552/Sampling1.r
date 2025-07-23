## ----echo=FALSE---------------------------------------------------------------

# Create 'truth'
  set.seed(3433)
  pop.weights = rnorm(n = 500, mean = 300, sd = 100)

#Population mean
  pop.mu.weights = mean(pop.weights)

#Population standard deviation
  pop.sd.weights = sd(pop.weights)
  
# Sample Size  
  N = length(pop.weights)

# histogram    
  hist(pop.weights, main = "Total Population (N = 500)",freq=TRUE,xlim=c(0,700),xlab="Bear Weight")  


## ----echo=TRUE----------------------------------------------------------------
  pop.weights[1:5]


## ----echo=TRUE----------------------------------------------------------------
# Sample Size
  n = 50

# Sample and estimate mean one time
  sample.1 = mean(
                  sample(pop.weights,
                         n,
                         replace = FALSE
                         )
                  )

# Calculate Sampling Error
  sample.1 - pop.mu.weights


## ----echo=TRUE----------------------------------------------------------------
# Create function to sample 50 units and take the mean
sample.mean.fn = function(target,n){
                                    mean(
                                         sample(target,n)
                                         )
                                   }
#Repeat the above function 20000 times
  set.seed(54343)
  mu.hat=replicate(20000,
                   sample.mean.fn(pop.weights,n)
                   )



## ----echo=FALSE---------------------------------------------------------------
par(mfrow=c(1,2))
hist(mu.hat,breaks=40,main = "Sampling Distribution \n of Sample Mean for n = 50",xlim=c(240,380))
abline(v=pop.mu.weights,lwd=3,col=2)
legend("topright",legend=c("Population Mean"),lwd=2,col=2,cex=0.8)
hist(mu.hat-pop.mu.weights,breaks=40,main = "Sampling Error for n = 50")


## ----echo=TRUE----------------------------------------------------------------
lower=pop.mu.weights - pop.mu.weights*0.10
upper=pop.mu.weights + pop.mu.weights*0.10

length(which(mu.hat>lower & mu.hat<upper)) / length(mu.hat)



## ----echo=FALSE---------------------------------------------------------------
  sample.pop.weights = pop.weights[which(pop.weights>300)]

par(mfrow=c(1,2))
  hist(pop.weights, main = "Total Population (N = 500)",freq=TRUE,xlim=c(0,500),xlab="Bear Weight")
  hist(sample.pop.weights, main = "Sample Population",col=2,freq=TRUE,xlim=c(0,500),xlab="Bear Weight")


## ----echo=FALSE---------------------------------------------------------------
#Repeat the above function 20000 times
set.seed(54343)
mu.hat=replicate(20000,sample.mean.fn(sample.pop.weights,n))
par(mfrow=c(1,2))
hist(mu.hat,breaks=40,main = "Sampling Distribution \n of Sample Mean for n = 50",
     xlim=c(200,400))
abline(v=pop.mu.weights,lwd=3,col=2)
legend("topleft",legend=c("Population Mean"),lwd=2,col=2)
hist(mu.hat-pop.mu.weights,breaks=40,main = "Sampling Bias for n = 50")


## ----echo=FALSE---------------------------------------------------------------
#Repeat the above function 20000 times
biased.sample.mean.fn = function(x,n){
                                  temp=sample(x,n)
                                  sum(temp^0.91/1.3)/length(temp)^(1/2)
}
set.seed(54343)
mu.hat=replicate(20000,biased.sample.mean.fn(pop.mu.weights,n))
par(mfrow=c(1,2))
hist(mu.hat,breaks=40,main = "Sampling Distribution \n of Sample Mean for n = 50",
     xlim=c(0,2000))
abline(v=pop.mu.weights,lwd=3,col=2)
legend("topright",legend=c("Population Mean"),lwd=2,col=2)
hist(mu.hat-pop.mu.weights,breaks=40,main = "Sampling Error for n = 50")


## ----echo=FALSE---------------------------------------------------------------
  N = 200 # number of streams
  prob.occ=0.5  #true prob. of fish occupancy for each stream
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


## -----------------------------------------------------------------------------
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


## ----echo=FALSE---------------------------------------------------------------
#Output code into R file
knitr::purl(input="../FW552/Sampling1.qmd",output="../FW552/Sampling1.r")

