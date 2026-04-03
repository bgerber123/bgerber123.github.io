## ----eval=TRUE,echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
set.seed(452)
y=rpois(1000,10)
main="Distribution of counts of plants \nin all possible plots"


## ----eval=TRUE,echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
#| code-line-numbers: 2,3
par(mfrow=c(1,2))
hist(y, breaks=20,xlim=c(0,25),main=main)
hist(y, breaks=20,xlim=c(0,25),freq = FALSE,main=main)


## ----eval=TRUE,echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
set.seed(452)
y=rpois(50,10)
hist(y, main="Sample of counts of plants (n=50)",breaks=10,xlim=c(0,25))
hist(y, main="Sample of counts of plants (n=50)",breaks=10,xlim=c(0,25),freq = FALSE)


## ----bern, echo=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------------------------
#Define inputs
  theta=0.2;  N=1 

#Random sample - 1 duck
  rbinom(n=1,size=N,theta)

#Random sample - 10 ducks
  rbinom(n=10,size=N,theta)


## ----bern2, echo=TRUE, eval=TRUE--------------------------------------------------------------------------------------------------------------------------
y.mat = replicate(1000,rbinom(n = 10,size=N,theta))
theta.hat = apply(y.mat, 2, mean)


## ----bern3, echo=FALSE, eval=TRUE,  out.width="75%"-------------------------------------------------------------------------------------------------------
hist(theta.hat,freq=TRUE,breaks=40, main=bquote("Sampling Distribution of"~theta),xlab=expression(theta))


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
# 1 duck tagged/released and one simulation
  theta=0.2;  N=1 
  rbinom(n=1,size=N,theta)


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
# 1000 ducks tagged/released and one simulation
  theta=0.2;  N=1000 
  rbinom(n=1,size=N,theta)


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
# 1000 ducks tagged/released and 10 simulation
  theta=0.2;  N=1000 
  rbinom(n=10,size=N,theta)


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
# 1 duck tagged for each of 1000 simulations
  theta=0.2;  N=1
  y = rbinom(n=1000,size=N,theta)
  y


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
sum(y)


## ----eval=TRUE,echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
shape=100
rate=1
scale=1/rate

set.seed(4454)
round(rgamma(10,shape,rate),digits=2)



## ----echo=FALSE,eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
mu=shape*scale
var=shape*scale^2
curve(dgamma(x, shape=shape,rate=rate),xlim=c(0,200),lwd=4,xlab="y",
      ylab="dgamma(y,shape = 100, rate = 1",main="Equivalent means and variances")
curve(dnorm(x, mu,sqrt(var)),add=TRUE,col=2,lwd=4,lty=2)
legend("topright",legend = c("Gamma PDF (shape = 100, rate = 1)", "Normal PDF (mu = 100, var = 100)"),lwd=3,col=c(1,2))


## ----echo=FALSE,eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
shape=1
rate=1
scale=1/rate

mu=shape*scale
var=shape*scale^2
curve(dgamma(x, shape=shape,rate=rate),xlim=c(-5,5),lwd=4,xlab="y",
      ylab="dgamma(y,shape = 1, rate = 1",main="Equivalent means and variances")
curve(dnorm(x, mu,sqrt(var)),add=TRUE,col=2,lwd=4,lty=2)
legend("topright",legend = c("Gamma PDF (shape = 1, rate = 1)", "Normal PDF (mu = 1, var = 1)"),lwd=3,col=c(1,2))


## ----echo=FALSE,eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
library(visualize)


## ----echo=TRUE,eval=TRUE----------------------------------------------------------------------------------------------------------------------------------
visualize.it(dist = 'norm', stat = c(100),
             list(mu = 100 , sd = 10), section = "upper")


## ----echo=FALSE,eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
library(visualize)
visualize.it(dist = 'norm', stat = c(120),
             list(mu = 100 , sd = 10), section = "upper")


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------
pnorm(120,mean=100,sd=10,lower.tail = FALSE)


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------
qnorm(0.02275,100,10,lower.tail = FALSE)


## ----pdf1, echo=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------------------------
y = rnorm(1000, mean = 20, sd = 3)
hist(y,freq=FALSE,ylim=c(0,0.14))
lines(density(y),lwd=3,col=4)


## ----pdf1b, echo=TRUE, eval=TRUE--------------------------------------------------------------------------------------------------------------------------
curve(dnorm(x, mean= 20, sd = 3),
      xlim=c(0,40),lwd=3,col=2,ylab="Probability Density",xlab="y")
abline(v=20, lwd=3, col=1, lty=4)


## ----pdf2a, echo=TRUE, eval=FALSE-------------------------------------------------------------------------------------------------------------------------
## curve(dnorm(x, mean = 10, sd = 4),xlim=c(0,40),lwd=4,col=3,add=TRUE)


## ----pdf2b, echo=FALSE, eval=TRUE-------------------------------------------------------------------------------------------------------------------------
curve(dnorm(x, mean = 20, sd = 3),xlim=c(0,40),lwd=3,col=2,ylab="Probability Density",xlab="y")
abline(v=20, lwd=3, col=1, lty=4)
curve(dnorm(x, mean= 10, sd = 4),xlim=c(0,40),lwd=4,col=3,add=TRUE)


## ----pdf3, echo=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------------------------
shape =10
scale = 2

mean1 = shape*scale
mean1

mode1 = (shape-1)*scale
mode1

stdev = sqrt(shape*scale^2)
stdev


## ----pdf4, echo=FALSE, eval=TRUE--------------------------------------------------------------------------------------------------------------------------
curve(dgamma(x, shape = shape, scale=scale),xlim=c(0,50),lwd=3,col=2,ylab="Probability Density",xlab="y")
abline(v=mean1, lwd=3, col=1, lty=4); abline(v=mode1, lwd=3, col=3, lty=4)
legend("topright",lty=3, col=c(1,2),legend=c("Mean","Mode"),lwd=3)


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
pgamma(q=40, shape=10, scale=2,lower.tail=FALSE)


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
pgamma(q=20,shape=10, scale=2,lower.tail=TRUE)


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
pgamma(q=40,shape=10, scale=2,lower.tail=TRUE)-
pgamma(q=20,shape=10, scale=2,lower.tail=TRUE)


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
qgamma(p=0.025,shape=10, scale=2,lower.tail=TRUE)


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
qgamma(p=0.025,shape=10, scale=2,lower.tail=FALSE)


## ----eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------------------------------------------------
curve(dgamma(x,shape=10, scale=2),xlim=c(0,50),lwd=3,
      xlab="y", ylab="dgamma(x,shape=10, scale=2)")
abline(v=c(9.590777,34.16961),lwd=3,col=2)


## ----echo=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
set.seed(154434)
y <- rgamma(100, shape=10, scale=2)


## ----echo=FALSE, eval=TRUE--------------------------------------------------------------------------------------------------------------------------------
curve(dgamma(x,shape=10, scale=2),xlim=c(0,50),lwd=3)
hist(y,col=adjustcolor("red",alpha.f = 0.5),freq=FALSE,add=TRUE,breaks=100)

