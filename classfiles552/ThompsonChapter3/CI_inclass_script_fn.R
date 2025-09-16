
# load data
dat = read.csv("Counts_Kangaroo.csv",colClasses = "integer")

# We are interested in constructing confidence intervals
# for the sample mean and for the estimate of total population siz
# of Kangaroos


head(dat)
dim(dat)

N = nrow(dat)  

hist(dat$Count)

# True population size
Total = sum(dat$Count)

# Population mean
mu =  mean(dat$Count)

# Randomly sample 

n = 20
n.sim = 2000


# Create a function that gets the CI's and whether it includes truth
myCI.func=function(Count,n){
  mu.indicator = 0
  total.indicator = 0
  
  # Type I error
  alpha = 0.1
  
  confidence.coef = 1-alpha
  
  # quantiles
  t.upper = confidence.coef+alpha/2
  t.lower = 0+alpha/2
  
  
  y = sample(Count,size=n)
  y.mean= mean(y)
  
  y.LCL = y.mean + qt(t.lower,df=length(y)-1) * sqrt(((N-n)/N)*(var(y)/n))
  y.UCL = y.mean + qt(t.upper,df=length(y)-1) * sqrt(((N-n)/N)*(var(y)/n))
  
  if (y.LCL<mu & mu < y.UCL) {mu.indicator = 1}
  
  tau = N*y.mean

  tau.LCL = tau + qt(t.lower,df=length(y)-1) * sqrt(N*(N-n)*var(y)/n)
  tau.UCL = tau + qt(t.upper,df=length(y)-1) * sqrt(N*(N-n)*var(y)/n)
  
  
  if (tau.LCL<Total & Total < tau.UCL) {total.indicator = 1}
  
  list(y.mean=y.mean,
       y.LCL=y.LCL,
       y.UCL=y.UCL,
       mu.indicator=mu.indicator,
       tau=tau,
       tau.LCL=tau.LCL,
       tau.UCL=tau.UCL,
       total.indicator=total.indicator
  )
       
}  
  
  
# Use replicate to use the function
save.outputs=replicate(n.sim,myCI.func(Count=dat$Count,n=n))
  
rownames(save.outputs)

mean(unlist(save.outputs[8,]))

library(ggplot2)
plot.outputs=NULL
plot.outputs$x= 1:n.sim
plot.outputs$mean= unlist(save.outputs[1,])
plot.outputs$mean.LCL= unlist(save.outputs[2,])
plot.outputs$mean.UCL= unlist(save.outputs[3,])

plot.outputs=data.frame(plot.outputs)

ggplot(data = plot.outputs, aes(x = x, y = mean)) +
  geom_ribbon(aes(ymin = mean.LCL, ymax = mean.UCL)) +
  geom_line()+ geom_hline(yintercept = mu,color="red")+
  ylim(0,20)


# Sampling distribution of the Confidence Intervals
hist(plot.outputs$mean.LCL)
hist(plot.outputs$mean.UCL)
