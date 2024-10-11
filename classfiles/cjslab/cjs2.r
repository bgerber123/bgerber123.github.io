model {
  
  # Priors
  beta0 ~ dlogis(0,1)
  beta1 ~ dlogis(0,1)
  p ~ dbeta(1, 1)
  
  # Likelihood 
  for (i in 1:nind){
    # Define latent state at first capture
    # Known to be alive on first occasion
    z[i,f[i]] <- 1
    
    logit(phi[i]) <- beta0 + beta1*sex[i]
    
    for (t in (f[i]+1):n.occasions){
      # State process
      z[i,t] ~ dbern(phi[i] * z[i,t-1])
      
      
      # Observation process
      y[i,t] ~ dbern(p * z[i,t])
    } #end loop t
    
    
  } #end loop i
  
# Derive probability of survival for male and female
# This is done outside of the loops as we are not using the indices
  male.phi <- phi[1]
  female.phi <- phi[2] 

}