## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
# x variable (independent and known)
  x = seq(0,2,by=0.1)
  x


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
# marginal coefficients (estimated if unknown)
  beta0 = 0
  beta1 = 0.5

#stdev of y, (estimated if unknown) 
  sigma = 0.5

# Derive mu  
  mu = beta0 + beta1*x

# Single random realization of y, 
#  based on mu and sigma  
  set.seed(43252)
  y =  rnorm(length(mu),mean = mu, sd = sigma)

#plot x and y  
  par(cex.axis=1.3,cex.lab=1.3)
  plot(x,y,type="p",col=4,lwd=4,ylim=c(-2,3),
       pch=18,cex=2)
  lines(x,mu,lwd=3)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
#sample many times  
  y.many = replicate(1000,rnorm(length(mu),mu, sigma))

#plot all samples with true mu
  par(cex.axis=1.3,cex.lab=1.3)
  matplot(y.many,type="p",pch=18,xaxt="n",col=2,cex=2,xlab="x",ylab="y")
  axis(1,at=1:length(x),lab=x)
  lines(1:length(x),mu,lwd=3)
  points(1:length(x),y,type="p",col=4,lwd=4,ylim=c(-2,3),
       pch=18,cex=2)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
y <- matrix(c(1,2,3),nrow=3,ncol=1)
y


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
x <- matrix(c(0.5,1,-2),nrow=3,ncol=1)
x


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
p=3
beta <- matrix(c(0,-2,2),nrow=p,ncol=1)
beta


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
p=2
X <- matrix(c(1,2,3,4,5,6),nrow=3,ncol=p,byrow=FALSE)
X


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
t(y)%*%y


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
first=t(y)
dim(first)

second=y
dim(second)

#When this is true
  ncol(first)==nrow(second)


## ---- eval=TRUE,echo=TRUE------------------------------------------------------
#Design matrix
  set.seed(6454)
  Var1 = seq(0,20,by=1)+rnorm(21,0,2)
  X = model.matrix(~Var1)
  head(X)
  


## ---- eval=TRUE,echo=TRUE------------------------------------------------------
#Marginal Coefficients  
  beta=matrix(c(0,5))

#linear terms
  lt = X%*%beta

#mu (no transformation needed)
  mu=lt

# Plot relationship b/w mean (mu) and variable of interest
  plot(X[,2],mu,type="b",lwd=4)  


## ---- eval=TRUE,echo=TRUE------------------------------------------------------
#sample
  set.seed(5435)
  y = rnorm(length(mu),mu)
  y


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
# Fit model to sample
  model1=glm(y~X+0,family = gaussian(link = "identity"))
  summary(model1)


## ----echo = TRUE---------------------------------------------------------------
library(equatiomatic)
extract_eq(model1)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
 #sample many times  
  y.many = replicate(1000,rnorm(length(mu),mu, sigma))
  dim(y.many)
  
 #Estimate coefs for all 100 samples
  coef.est=apply(y.many,2,FUN=function(y){
              model1=glm(y~X+0,family = gaussian(link = "identity"))
              model1$coefficients
  })

  dim(coef.est)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
  plot(density(coef.est[1,],adjust=2),type="l",lwd=2,
       main=bquote("Sampling Distribution"~beta[0]),
       xlab=bquote(beta[0]))
      abline(v=beta[1],col=2,lwd=4)
  plot(density(coef.est[2,],adjust=2),type="l",lwd=2,
       main=bquote("Sampling Distribution"~beta[1]),
       xlab=bquote(beta[1]))
  abline(v=beta[2],col=2,lwd=4)


## ---- eval=TRUE,echo=TRUE, fig.align='center'----------------------------------
p=seq(0.01,0.99,by=0.01)
logit.p=qlogis(p)
par(cex.lab=1.5,cex.axis=1.5)
plot(p,logit.p,type="l",lwd=4,col=3,xlab='p',ylab="logit(p)")


## ---- eval=TRUE,echo=TRUE, fig.align='center'----------------------------------
#Design matrix
  set.seed(43534)
  Var1 = rnorm(20)
  X = model.matrix(~Var1)
  head(X)


## ---- eval=TRUE,echo=TRUE, fig.align='center'----------------------------------
# marginal coefficients (on logit-scale)
  beta=c(-4,8)

#linear terms
  lt = X%*%beta

#transformation via link function to probability scale
  p=plogis(lt)
  head(round(p,digits=2))

#sample
  set.seed(14353)
  y = rbinom(n=length(p),size=1,p)
  y


## ---- eval=TRUE,echo=TRUE, fig.align='center'----------------------------------
#Plot the linear 'terms'  and probability
  par(cex.lab=1.2,cex.axis=1.2)
  plot(Var1,lt,type="b",lwd=3,col=2,
       xlab="x",ylab="Linear Terms (logit-value)")  

  index=order(Var1)
  plot(Var1[index],p[index],type="b",lwd=3,col=2,xlab="x",ylab="Probability")  


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
# Fit model to sample 
  model1=glm(y~X+0, family = binomial(link = "logit"))
  summary(model1)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
 #sample many times  
  y.many = replicate(1000,rbinom(n=length(p),size=1,p))
  dim(y.many)
  
 #Estimate coefs for all 100 samples
  coef.est=apply(y.many,2,FUN=function(y){
          model1=glm(y~X+0, family = binomial(link = "logit"))
          
  model1$coefficients
  })

  dim(coef.est)


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
  plot(density(coef.est[1,],adjust=2),type="l",lwd=2,
       main=bquote("Sampling Distribution"~beta[0]),
       xlab=bquote(beta[0]))
      abline(v=beta[1],col=2,lwd=4)
  plot(density(coef.est[2,],adjust=2),type="l",lwd=2,
       main=bquote("Sampling Distribution"~beta[1]),
       xlab=bquote(beta[0]))
  abline(v=beta[2],col=2,lwd=4)

