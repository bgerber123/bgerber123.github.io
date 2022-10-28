#Setup the workspace
  rm(list=ls())
  library(rstanarm)
  library(bayesplot)

#Hippo survival data 
  y=c(0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)

#Create a datafame
  dat=data.frame(y=y)

#Fit model in stan_glm function. Note, the lack of a prior specifiction.
  post1 <- stan_glm(y~1, data = dat,
                 family = binomial(link = "logit"))

  
    
#Summarize the estimate coefs and predicted/fitted values
  plot(post1)  
  summary(post1)
  post1$coefficients
  plogis(post1$coefficients)

  post1$fitted.values

#Plot all chains MCMC iterations
  color_scheme_set("blue")
  mcmc_trace(post1)
  
  #Posterior mean and median
  mean(as.matrix(post))
  median(as.matrix(post))    

#95% Credible Intervals
  quantile(as.matrix(post),probs=c(0.025,0.975))


    

