model {
  
  # Priors
  phi ~ dbeta(1, 1)
  p ~ dbeta(1, 1)
  
  # Likelihood 
  for (i in 1:nind){
    # Define latent state at first capture
    # Known to be alive on first occasion
    z[i,f[i]] <- 1
    
    for (t in (f[i]+1):n.occasions){
      # State process
      z[i,t] ~ dbern(phi * z[i,t-1])
      
      # Observation process
      y[i,t] ~ dbern(p * z[i,t])
    } #end loop t
  } #end loop i
}