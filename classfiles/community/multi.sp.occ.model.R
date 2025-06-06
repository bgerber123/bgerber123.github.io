model {
  
  #Priors
  omega ~ dunif(0,1)
  
  psi.mean ~ dunif(0,1)
  beta <- log(psi.mean) - log(1-psi.mean)
  
  p.mean ~ dunif(0,1)
  #alpha <- log(p.mean) - log(1-p.mean)
  eta~dbeta(1,1)
  
  sigma.u ~ dunif(0,10)
  sigma.v ~ dunif(0,10)
  tau.u <- pow(sigma.u,-2)
  tau.v <- pow(sigma.v,-2)
  
  rho ~ dunif(-1,1)
  var.eta <- tau.v/(1.-pow(rho,2))
  
  
  for (i in 1:(n+nzeros)) {
    w[i] ~ dbern(omega)
    phi[i] ~ dnorm(beta, tau.u)#T(-20,20)
    
    #mu.eta[i] <- alpha + (rho*sigma.v/sigma.u)*(phi[i] - beta)
    #eta[i] ~ dnorm(mu.eta[i], var.eta)#T(-20,20)
    
    
    logit(psi[i]) <- phi[i]
    logit(p[i]) <- eta
    
    mu.psi[i] <- psi[i]*w[i]
    for (k in 1:R) {
      Z[i,k] ~ dbern(mu.psi[i])
      mu.p[i,k] <- p[i]*Z[i,k]
      Y[i,k] ~ dbin(mu.p[i,k], J)
    }
  }
  
  n0 <- sum(w[(n+1):(n+nzeros)])
  N <- n + n0
}
