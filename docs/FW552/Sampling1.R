## ----echo=FALSE---------------------------------------------------------------

# Create 'truth'
  set.seed(3433)
  pop.weights = rnorm(n = 500, mean = 300, sd = 50)

#Population mean
  pop.mu.weights = mean(pop.weights)

#Population standard deviation
  pop.sd.weights = sd(pop.weights)
  
# Sample Size  
  N = length(pop.weights)

# histogram    
  hist(pop.weights, main = "Total Population (N = 500)",freq=TRUE,xlim=c(0,700),xlab="Bear Weight (lbs)")  


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
# Repeat the above function 20000 times
  mu.hat = replicate(20000,
                     sample.mean.fn(pop.weights,n)
                     )



## ----echo=FALSE---------------------------------------------------------------
par(mfrow=c(1,2))
hist(mu.hat,breaks=40,main = "Sampling Distribution \n of Sample Mean for n = 50",xlim=c(240,380),
     xlab=expression(hat(mu)))
abline(v=pop.mu.weights,lwd=3,col=2)
legend("topright",legend=c("Population Mean"),lwd=2,col=2,cex=0.8)
hist(mu.hat-pop.mu.weights,breaks=40,main = "Sampling Error for n = 50",
     xlab=expression(hat(mu)- mu))


## ----echo=FALSE---------------------------------------------------------------
hist(mu.hat,breaks=40,main = "Sampling Distribution \n of Sample Mean for n = 50",xlim=c(240,380),
     xlab=expression(hat(mu)))
abline(v=pop.mu.weights,lwd=3,col=2)


## ----echo=FALSE---------------------------------------------------------------
hist(mu.hat,breaks=40,main = "Sampling Distribution \n of Sample Mean for n = 50",xlim=c(240,380),
     xlab=expression(hat(mu)))
abline(v=pop.mu.weights,lwd=3,col=2)


## ----echo=TRUE----------------------------------------------------------------
lower = pop.mu.weights - pop.mu.weights*0.05
upper = pop.mu.weights + pop.mu.weights*0.05

length(which(mu.hat > lower & mu.hat < upper)) / length(mu.hat)



## ----echo=FALSE---------------------------------------------------------------
par(mfrow=c(2,2))
curve(dnorm(x, 300, 10),lwd=3,xlim=c(150,450),main="Unbiased & Accurate",xlab="Bear Weight (lbs)",
      ylab="Density")
abline(v=300,lwd=3,col=2)

curve(dnorm(x, 295, 10),lwd=3,xlim=c(150,450),main="Biased & Accurate",xlab="Bear Weight (lbs)",
      ylab="Density")
abline(v=300,lwd=3,col=2)

curve(dnorm(x, 290, 10),lwd=3,xlim=c(150,450),main="Biased & Inaccurate",xlab="Bear Weight (lbs)",
      ylab="Density")
abline(v=300,lwd=3,col=2)

curve(dnorm(x, 270, 10),lwd=3,xlim=c(150,450),main="Biased & Inaccurate",xlab="Bear Weight (lbs)",
      ylab="Density")
abline(v=300,lwd=3,col=2)


## ----echo=FALSE---------------------------------------------------------------
par(mfrow=c(1,1))
curve(dnorm(x, 300, 50),lwd=3,xlim=c(150,450),ylim=c(0,0.04),xlab="Bear Weight (lbs)")
curve(dnorm(x, 310, 10),lwd=3,add=TRUE,col=3)
legend("topright",lwd=3,col=c(1,3,2),legend=c("Unbiased/Accurate/Imprecise",
                                            "Biased/Accurate/Precise",
                                            expression(mu)))
abline(v=300,lwd=3,col=2)


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
     xlim=c(200,400),xlab=expression(hat(mu)))
abline(v=pop.mu.weights,lwd=3,col=2)
legend("topleft",legend=c("Population Mean"),lwd=2,col=2)
hist(mu.hat-pop.mu.weights,breaks=40,main = "Sampling Bias for n = 50",xlab=expression(hat(mu)- mu))


## ----echo=FALSE---------------------------------------------------------------
#Repeat the above function 20000 times
biased.sample.mean.fn = function(x,n){
                                  temp=sample(x,n)
                                  sum(temp^0.38)/length(temp)^(1/10000)
}
set.seed(54343)
mu.hat=replicate(20000,biased.sample.mean.fn(pop.mu.weights,n))
par(mfrow=c(1,2))
hist(mu.hat,breaks=40,main = "Sampling Distribution \n of Sample Mean for n = 50",
     xlim=c(0,600))
abline(v=pop.mu.weights,lwd=3,col=2)
legend("topright",legend=c("Population Mean"),lwd=2,col=2,cex=0.8)
hist(mu.hat-pop.mu.weights,breaks=40,main = "Sampling Error for n = 50")


## ----echo=FALSE, results='hide'-----------------------------------------------
#Output code into R file
knitr::purl(input="../FW552/Sampling1.qmd",output="../FW552/Sampling1.r")

