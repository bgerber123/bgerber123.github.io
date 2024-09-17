model {
	# Priors
	b0~dlogis(0,1)
	a0~dlogis(0,1)
	
  # Loop over sites	
	for(i in 1:n.sites){
		logit(psi[i]) <- b0 
		z[i] ~ dbern(psi[i])
		# Loop over occasions within sites			
		for(j in 1:n.visits){
			logit(p[i,j]) <- a0
			peff[i,j] <- p[i,j]*z[i]
			y[i,j] ~ dbern(peff[i,j])
		}# j loop
	} #i loop
}