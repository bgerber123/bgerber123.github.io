model {
	# Priors
	psi~dunif(0,1)
	p~dunif(0,1)
	
  # Loop over sites	
	for(i in 1:n.sites){
		z[i] ~ dbern(psi)
		# Loop over occasions within sites			
		for(j in 1:n.visits){
			y[i,j] ~ dbern(p*z[i])
		}# j loop
	} #i loop
}