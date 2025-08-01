
# load data
 dat = read.csv("Counts_Kangaroo.csv",colClasses = "integer")

# We are interested in constructing confidence intervals
# for the sample mean and for the estimate of total population size
# of Kangaroos
 

  head(dat)
  dim(dat)
  
#There are 144 locations
  N = nrow(dat)  
 
# Distribution of true counts of Kangaroos   
  hist(dat$Count)
  
# True population size
  Total = sum(dat$Count)
  
# Population mean
  mu =  mean(dat$Count)

  
# Lets grab one simple random sample of size 20
  n = 20
  
# Type I error
  alpha = 0.05

# Confidence  Coefficient
  confidence.coef = 1-alpha    
  
# Get quantiles of the t-distribution
  q.upper = confidence.coef+alpha/2
  q.lower = 0+alpha/2
  
  
# Create 95% confidence intervals for sample mean
  
# Create 95% confidence intervals for the total population size
  
  
#######################  
  
# Lets create a simulation to asses whether our SRS is working and our 
#  95% confidence intervals mean what we hope they do.
  
# How many simulations do we want
  n.sim = 2000
  
  
# Randomly sample 
  
  n = 20

    
# Create a data.frame of outputs to save  
  save.outputs <- data.frame(matrix(ncol = 9, nrow = n.sim))
  
  colnames(save.outputs) <- c("sim.iter","mean", "mean.LCL", "mean.UCL", "mean.indicator",
                              "tau", "tau.LCL", "tau.UCL", "total.indicator"
                              )
  for(i in 1:n.sim){
    
    STUFF
    
  }
  

  
library(ggplot2)

save.outputs$x= 1:n.sim

ggplot(data = save.outputs, aes(x = x, y = mean)) +
         geom_ribbon(aes(ymin = mean.LCL, ymax = mean.UCL)) +
         geom_line()+ geom_hline(yintercept = mu,color="red")+
         ylim(0,20)
  
