
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
  
# Type I error
  alpha = 0.1
  
  confidence.coef = 1-alpha

  # quantiles
  t.upper = confidence.coef+alpha/2
  t.lower = 0+alpha/2

  

save.outputs <- data.frame(matrix(ncol = 8, nrow = n.sim))

colnames(save.outputs) <- c("mean", "mean.LCL", "mean.UCL", "mean.indicator",
                            "tau", "tau.LCL", "tau.UCL", "total.indicator"
                            )

for(i in 1:n.sim){
  mu.indicator = 0
  total.indicator = 0
  
  y = sample(dat$Count,size=n)
  y.mean= mean(y)
  
  y.LCL = y.mean + qt(t.lower,df=length(y)-1) * sqrt(((N-n)/N)*(var(y)/n))
  y.UCL = y.mean + qt(t.upper,df=length(y)-1) * sqrt(((N-n)/N)*(var(y)/n))
  
 if (y.LCL<mu & mu < y.UCL) {mu.indicator = 1}
  
  tau = N*y.mean
  
  
  tau.LCL = tau + qt(t.lower,df=length(y)-1) * sqrt(N*(N-n)*var(y)/n)
  tau.UCL = tau + qt(t.upper,df=length(y)-1) * sqrt(N*(N-n)*var(y)/n)
  
  
  if (tau.LCL<Total & Total < tau.UCL) {total.indicator = 1}

  save.outputs$mean[i] = y.mean
  save.outputs$mean.LCL[i] = y.LCL
  save.outputs$mean.UCL[i] = y.UCL
  save.outputs$mean.indicator[i] = mu.indicator
  
  save.outputs$tau[i] = tau
  save.outputs$tau.LCL[i] = tau.LCL
  save.outputs$tau.UCL[i] = tau.UCL
  save.outputs$total.indicator[i] = total.indicator
  
}

head(save.outputs)


mean(save.outputs$total.indicator)

mean(save.outputs$mean.indicator)



library(ggplot2)

save.outputs$x= 1:n.sim

ggplot(data = save.outputs, aes(x = x, y = mean)) +
         geom_ribbon(aes(ymin = mean.LCL, ymax = mean.UCL)) +
         geom_line()+ geom_hline(yintercept = mu,color="red")+
         ylim(0,20)
  
