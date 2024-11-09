#Code and setup from Hierarchical Modeling and Inference in Ecology, Chapter 12 
  Ymat = as.matrix(read.csv('detectionFreq.NH17.csv'))
  
  
#rows= species, columns = total detections at each site  
  head(Ymat)
  
# Number of sampling days visited by a single observer in this BBS route  
  nrepls = 11

  # augment data matrix with an arbitrarily large number of zero row vectors
  nzeros = 151
  n = dim(Ymat)[1]
  nsites = dim(Ymat)[2]
  Yaug = rbind(Ymat, matrix(0, nrow=nzeros, ncol=nsites))

  # JAGS data
  params = list('alpha', 'beta', 'rho', 'sigma.u', 'sigma.v', 'omega', 'N', 'phi','eta')

  inits = function() {
    omegaGuess = 1
    psi.meanGuess = 1
    p.meanGuess = 0.1
    rhoGuess = 0.5
    sigma.uGuess =  0.5
    sigma.vGuess =  0.5
    list(omega=omegaGuess, psi.mean=psi.meanGuess, p.mean=p.meanGuess,
         sigma.u=sigma.uGuess, sigma.v=sigma.vGuess, rho=rhoGuess,
         w=c(rep(1, n), rbinom(nzeros, size=1, prob=omegaGuess)),
         phi=rnorm(n+nzeros, log(psi.meanGuess/(1-psi.meanGuess)), sigma.uGuess),
        # eta=rnorm(n+nzeros, log(p.meanGuess/(1-p.meanGuess)), sigma.vGuess),
         Z = matrix(rbinom((n+nzeros)*nsites, size=1, prob=psi.meanGuess), nrow=(n+nzeros))
         #Z = matrix(1, nrow=nrow(Yaug), ncol=ncol(Yaug))
         )
  }
  
 
  
  
  
  #JAGS data list
  data = list(
    n=n, nzeros=nzeros, R=nsites, J=nrepls, Y=Yaug
  )
  
  #MCMC inputs  
  n.chains=3
  n.adapt=4000
  n.iter=15000
  thin=3
  burn=4000
  

  # Setup the Model
  jm=jags.model(file="multi.sp.occ.model.R", data=data,n.chains=n.chains,n.adapt=n.adapt,inits=inits)
  
  # Update the model with the burnin
  update(jm, n.iter=burn)
  
  #Fit the modedl  
  post=coda.samples(jm, variable.names=params, n.iter=n.iter, thin=thin)
  
  #save(post,file="post.extra.var")
  #load("post")
  
  #Look at chains
  #Plot all chains MCMC iterations
  color_scheme_set("viridis")
  mcmc_trace(post)
  
  

