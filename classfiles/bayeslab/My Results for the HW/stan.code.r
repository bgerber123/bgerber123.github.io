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

# posterior predictions of the probability  
# each column is for each observations (all same) 
# as this is the intercept model. Rows are the mcmc iterations
  preds=rstanarm::posterior_epred(post1)
  hist(preds[,1])
    
#Plot all chains MCMC iterations
  color_scheme_set("viridis")
  mcmc_trace(post1)
  
  #Posterior mean and median
  mean(as.matrix(post1))
  median(as.matrix(post1))    

#95% Credible Intervals
  quantile(as.matrix(post1),probs=c(0.025,0.975))


    

