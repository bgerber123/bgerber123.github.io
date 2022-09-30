## ---- eval=TRUE,echo=FALSE----------------------------------------------------------------------
set.seed(452)
y=rpois(1000,10)
main="Distribution of counts of plants \nin all possible plots"


## ---- eval=TRUE,echo=TRUE-----------------------------------------------------------------------
par(mfrow=c(1,2))
hist(y, breaks=20,xlim=c(0,25),main=main)
hist(y, breaks=20,xlim=c(0,25),freq = FALSE,main=main)


## ---- eval=TRUE,echo=FALSE----------------------------------------------------------------------
par(mfrow=c(1,2))
set.seed(452)
y=rpois(50,10)
hist(y, main="Sample of counts of plants (n=50)",breaks=10,xlim=c(0,25))
hist(y, main="Sample of counts of plants (n=50)",breaks=10,xlim=c(0,25),freq = FALSE)


## ----echo=TRUE----------------------------------------------------------------------------------
#Bernoulli Sampling with prob of 0.5
  theta=0.5;  N=1 #1 duck tagged and released
  rbinom(n=1,size=N,theta)

#Binomial Sampling with prob of 0.5
  theta=0.5;  N=1000 #1000 ducks tagged and released
  set.seed(543531); rbinom(n=1,size=N,theta)


## ----eval=TRUE,echo=FALSE-----------------------------------------------------------------------
shape=100
rate=1
scale=1/rate

set.seed(4454)
round(rgamma(10,shape,rate),digits=2)



## ---- echo=FALSE,eval=TRUE----------------------------------------------------------------------
mu=shape*scale
var=shape*scale^2
curve(dgamma(x, shape=shape,rate=rate),xlim=c(0,200),lwd=4,xlab="y",
      ylab="dgamma(y,shape = 100, rate = 1",main="Equivalent means and variances")
curve(dnorm(x, mu,sqrt(var)),add=TRUE,col=2,lwd=4,lty=2)
legend("topright",legend = c("Gamma PDF (shape = 1, rate = 1)", "Normal PDF (mu = 100, var = 100)"),lwd=3,col=c(1,2))


## ---- echo=FALSE,eval=TRUE----------------------------------------------------------------------
shape=1
rate=1
scale=1/rate

mu=shape*scale
var=shape*scale^2
curve(dgamma(x, shape=shape,rate=rate),xlim=c(-5,5),lwd=4,xlab="y",
      ylab="dgamma(y,shape = 1, rate = 1",main="Equivalent means and variances")
curve(dnorm(x, mu,sqrt(var)),add=TRUE,col=2,lwd=4,lty=2)
legend("topright",legend = c("Gamma", "Normal"),lwd=3,col=c(1,2))


## ----echo=TRUE,eval=TRUE------------------------------------------------------------------------
library(visualize)
visualize.it(dist = 'norm', stat = c(100),
             list(mu = 100 , sd = 10), section = "upper")


## ----echo=FALSE,eval=TRUE-----------------------------------------------------------------------
library(visualize)
visualize.it(dist = 'norm', stat = c(120),
             list(mu = 100 , sd = 10), section = "upper")


## ---- echo=TRUE---------------------------------------------------------------------------------
pnorm(120,mean=100,sd=10,lower.tail = FALSE)


## ---- echo=TRUE---------------------------------------------------------------------------------
qnorm(0.02275,100,10,lower.tail = FALSE)


## ----eval=TRUE, echo=TRUE-----------------------------------------------------------------------
plnorm(q=600,meanlog=5, sdlog=1,lower.tail=FALSE)


## ----eval=TRUE, echo=TRUE-----------------------------------------------------------------------
plnorm(q=50,meanlog=5, sdlog=1,lower.tail=TRUE)


## ----eval=TRUE, echo=TRUE-----------------------------------------------------------------------
qlnorm(p=0.025,meanlog=5, sdlog=1,lower.tail=TRUE)


## ----eval=TRUE, echo=TRUE-----------------------------------------------------------------------
qlnorm(p=0.025,meanlog=5, sdlog=1,lower.tail=FALSE)


## ----eval=TRUE, echo=TRUE-----------------------------------------------------------------------
curve(dlnorm(x,meanlog=5, sdlog=1),xlim=c(0,1200),lwd=3,
      xlab="y", ylab="dlnorm(y,meanlog = 5, sdlog = 1)")
abline(v=c(20.9,1053.60),lwd=3,col=2)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------
set.seed(154434)
y <- rlnorm(100, meanlog = 5, sdlog = 1)


## ----echo=FALSE, eval=TRUE----------------------------------------------------------------------
curve(dlnorm(x,meanlog=5, sdlog=1),xlim=c(0,800),lwd=3)
hist(y,col=adjustcolor("red",alpha.f = 0.5),freq=FALSE,add=TRUE,breaks=100)


## ----eval=TRUE,echo=TRUE------------------------------------------------------------------------
# A data point
  y=c(10)

#the likelihood the mean is 8, given our data
  dnorm(y,mean=8)


## ----eval=TRUE,echo=TRUE------------------------------------------------------------------------
# Let's take many guesses of the mean
  means=seq(0,20,by=0.1)

# Use dnorm to get likelihood of each guess of the mean
# Assumes sd = 1
  likelihood=dnorm(y, mean=means)


## ----eval=TRUE,echo=FALSE, fig.align='center'---------------------------------------------------
#Look at gueses and likelihood
  plot(means,likelihood,xlab="Guesses for the Mean")
  abline(v=10,lwd=3,col=3)
  legend("topright",legend=c("Max Likelihood"),
         lwd=3,col=3)


## ---- echo=TRUE, eval=TRUE----------------------------------------------------------------------
# penguin height data
  y=c(4.34, 3.53, 3.75)

#Joint likelihood of mu=3, sigma =1, given our data
  prod(dnorm(y,mean=3,sd=1))


## ---- echo=TRUE, eval=TRUE----------------------------------------------------------------------

# The Guesses
  mu=seq(0,6,0.05)
  sigma=seq(0.01,2,0.05)
  try=expand.grid(mu,sigma)
  colnames(try)=c("mu","sigma")

# function
fun=function(a,b){
  prod(dnorm(y,mean=a,sd=b))
  }

# mapply the function with the inputs
  loglik=mapply(a=try$mu,b=try$sigma, FUN=fun)

# maximum likelihood of parameters
  try[which.max(loglik),]



## ----echo=FALSE,eval=TRUE, out.width="150%"-----------------------------------------------------
library(plotly)
f <- list(
    size = 15,
    family = 'sans-serif'
  )
  m <- list(
    l = 2,
    r = 0,
    b = 2,
    t = 2,
    pad = 0
  )
all=cbind(try,loglik)
colnames(all)=c("mu","sigma","likelihood")
fig <- plot_ly(all, x = ~mu, y = ~sigma, z = ~likelihood, marker = list(size = 5),width = 800, height = 800)
fig <- fig %>% add_markers(color=~likelihood)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'mu'),
                     yaxis = list(title = 'sigma'),
                     zaxis = list(title = 'Likelihood')))
fig %>% layout(font = f, margin = m)

#fig


## ----eval=TRUE,echo=FALSE-----------------------------------------------------------------------
set.seed(154541)
y=rnorm(100,3.8,1)

try=expand.grid(seq(0,6,0.01),seq(0.01,2,0.01))
  colnames(try)=c("mu","sigma")

# mapply the function with the inputs
  loglik=mapply(try$mu,try$sigma, FUN=fun)

  library(plotly)
f <- list(
    size = 15,
    family = 'sans-serif'
  )
  m <- list(
    l = 2,
    r = 0,
    b = 2,
    t = 2,
    pad = 0
  )
all=cbind(try,loglik)
colnames(all)=c("mu","sigma","likelihood")
fig <- plot_ly(all, x = ~mu, y = ~sigma, z = ~likelihood, marker = list(size = 5),width = 800, height = 800)
fig <- fig %>% add_markers(color=~likelihood)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Mean'),
                     yaxis = list(title = 'SD'),
                     zaxis = list(title = 'Likelihood')))
fig %>% layout(font = f, margin = m)

  


## ----echo=TRUE----------------------------------------------------------------------------------

#Note: optim function uses minimization, not maximization. 
#THUS, need to put negative in our function

#Note: log=TRUE, allows us to add rather than multiply 
#      (sum, instead of prod)

neg.log.likelihood=function(par){
  -sum(dnorm(y,mean=par[1],sd=par[2],log=TRUE))
  }

#find the values that minimizes the function
#c(1,1) are the initial values for mu and sigma
fit <- optim(par=c(1,1), fn=neg.log.likelihood,
             method="L-BFGS-B",
             lower=c(0,0),upper=c(10,1))

#Maximum likihood estimates for mu and sigma
fit$par

#The variance/dispersion parmeter is
(fit$par[2])^2




## ----echo=TRUE----------------------------------------------------------------------------------
out=glm(y~1,family=gaussian(link = "identity"))
summary(out)


## ---- eval=TRUE, echo=TRUE----------------------------------------------------------------------
#Parameters of Normal Distribution
  mu=10
  sd=9

#simualte and plot
  y = rnorm(10000,mean=mu,sd=sd)
  hist(y, main="Daily Rainfall")


## ---- eval=TRUE, echo=TRUE----------------------------------------------------------------------
#Parameters of Gamma Distribution
  alpha = mu^2/sd^2 
  beta = mu/sd^2

#simualte and plot
  y = rgamma(10000,shape=alpha, rate=beta)
  hist(y, main="Daily Rainfall")

