## ---- echo = TRUE-------------------------------------------------------
y <- c(0,1,1,1,1,0,1,0,0,0)
n <- length(y)
n


## ----echo=TRUE----------------------------------------------------------
#Bernoulli probability function
  prob.function=function(theta){prod(theta^y*(1-theta)^y)}

#possible probabilities
  theta.guess=matrix(seq(0.01,0.99,by=0.01))

#implement function
  likelihood=apply(theta.guess,1,prob.function)
  
#Find maximum likelood
  max.index=which.max(likelihood)

#Theta that maximizes our probability function
  theta.est=theta.guess[max.index]

#Define other probability
  q.est <- 1-theta.est

#Alternative estimation
  theta.est2 <- sum(y)/n

  


## ----echo=TRUE----------------------------------------------------------
#plot likelihood profile
  plot(theta.guess,likelihood)
  abline(v=theta.est,lwd=3,col=1,lty=1)
  abline(v=theta.est2,lwd=3,col=4,lty=4)
  legend("topright",lwd=3,col=c(1,4),
         legend=c("theta.est","theta.est2"))


## ----echo=TRUE----------------------------------------------------------
I = n/(theta.est*q.est)
I


## ----echo=TRUE, fig.height=6,fig.width=12, fig.align='center'-----------
thetas=seq(0.05,0.95,0.1)

Information <- n/(thetas*(1-thetas))

par(cex.lab=2.5,cex.axis=2.5,mar=c(5,5,2,2))
plot(thetas,Information,type="b",lwd=8)

abline(v=theta.est,col=2,lwd=8)


## ----echo=TRUE,fig.align='center',fig.width=12--------------------------
thetas=seq(0.05,0.95,0.1)

Var.theta <- (thetas*(1-thetas))/n
par(cex.main=2.5,cex.axis=2.5,cex.lab=2.5,mar=c(5,5,2,2))
plot(thetas,Var.theta,type="b",lwd=8)
abline(v=theta.est,col=2,lwd=8)

