# Author: Brian Gerber

# Date Last Modified: 10/20/2025

# Investigate the benefit of the ratio estimator compared to a sample mean
# Do this by varying sigma which will affect the correlation between y and x.


# Ratio estimator function

ratio.fn = function(N,x,y,mu.x){
  n = length(y)
  r = mean(y)/mean(x)
  mu = r * mu.x
  var.r = 1/(n-1) * sum((y-r*x)^2)
  var.mu = ((N-n)/N)*var.r/n
  
  list(mu = mu,
       var.mu = var.mu)
}


# Simulate across vary values of sigma
  sigma = c(0.01,0.1,1,10,100,200)
  nsim = 1000
  ratio.save = matrix(NA, nrow=nsim, ncol=length(sigma))
  avg.save = matrix(NA, nrow=nsim, ncol=length(sigma))
  cor.save = rep(NA, length(sigma))
  
for(i in 1:length(sigma)){
  
# Consider sample size of y/x effects on sampling distribution
  N = 1000
  x = rnorm(N, 100,10)
  beta=3
  epsilon = rnorm(N, 0, sigma[i])
  y = 0 + beta*x + epsilon

  cor.save[i] = cor(x,y)
  
# True values
  mu.x = mean(x)
  mu.y = mean(y)  
# Plot x-y
 # plot(x,y)

# Start the simulation
  for(j in 1:nsim){
  
# Sample only the quantiles
  n = 10
  
# Sample randomly
  random.index = sample(1:N,n)
  y.sample = y[random.index]
  x.sample = x[random.index]

# Estimate

  ratio.save[j,i] = ratio.fn(N,
                             x.sample,
                             y.sample,
                             mu.x)$mu 
  avg.save[j,i] = mean(y.sample)
  
  }
  print(i)
  }
  
  
#################
#Derive sampling distribution standard deviation
  

  apply(avg.save,2,sd)

  plot(cor.save,apply(ratio.save,2,sd),type = "l",lwd=2,xlab="Correlation x-y",ylab="Sampling Distribution SD")
  lines(cor.save,apply(avg.save,2,sd),type = "l",lwd=2,col=2)
  legend("topright",lwd=3,col=c(1,2),legend=c("Ratio","Sample Avg"))


###############################
###############################
###############################
# Investigate the bias of varying intercepts


# Simulate across vary values of sigma
sigma = 10
nsim = 1000
beta0 = c(0,5,10,20,40,80,160)
ratio.save = matrix(NA, nrow=nsim, ncol=length(beta0))
avg.save = matrix(NA, nrow=nsim, ncol=length(beta0))
mean.y=cor.save = rep(NA, length(beta0))

for(i in 1:length(beta0)){
  
  # Consider sample size of y/x effects on sampling distribution
  N = 1000
  set.seed(5435)
  x = rnorm(N, 100,10)
  beta1=3
  set.seed(5435545)
  epsilon = rnorm(N, 0, sigma)
  y = beta0[i] + beta1*x + epsilon
  mean.y[i] = mean(y)
  cor.save[i] = cor(x,y)
  
  # True values
  mu.x = mean(x)
  mu.y = mean(y)  
  # Plot x-y
  # plot(x,y)
  
  # Start the simulation
  for(j in 1:nsim){
    
    # Sample only the quantiles
    n = 10
    
    # Sample randomly
    random.index = sample(1:N,n)
    y.sample = y[random.index]
    x.sample = x[random.index]
    
    # Estimate
    
    ratio.save[j,i] = ratio.fn(N,
                               x.sample,
                               y.sample,
                               mu.x)$mu 
    avg.save[j,i] = mean(y.sample)
    
  }
  print(i)
}
# End sim

cor.save



#Calculate mean absolute bias and plot it against beta0
  plot(beta0,(apply(ratio.save,2,mean)-mean.y),type = "l",lwd=2)


#Calculate mean relative bias and plot it against beta0
  plot(beta0,(apply(ratio.save,2,mean)-mean.y)/mean.y,type = "l",lwd=2)




