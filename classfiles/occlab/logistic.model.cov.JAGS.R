model {
	# Priors
	b0~dlogis(0,1)
  b1~dlogis(0,1)

  # Loop over sites	
	for(i in 1:n.sites){
		logit(psi[i]) <- b0+b1*veg[i] 
		y[i] ~ dbern(psi[i])
	} #i loop
}