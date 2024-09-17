model {
	# Priors
  for(i in 1:n.beta){
    beta[i]~dlogis(0,1)
  }
  for(i in 1:n.alpha){
    alpha[i]~dlogis(0,1)
  }
  
  # Loop over sites	
	for(i in 1:n.sites){
		logit(psi[i]) <-  inprod(X[i,], beta)
		z[i] ~ dbern(psi[i])
		# Loop over occasions within sites			
		for(j in 1:n.visits){
			logit(p[i,j]) <- inprod(W[i,], alpha)
			peff[i,j] <- p[i,j]*z[i]
			y[i,j] ~ dbern(peff[i,j])
		}# j loop
	} #i loop
}