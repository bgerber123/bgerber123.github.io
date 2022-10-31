## ---- eval=TRUE, echo=FALSE-----------------------
# x variable (independent and known)
  x = seq(0,2,by=0.1)

# marginal coefficients 
  beta0 = 1
  beta1 = 0.5

# sd of y
  sigma = 0.5

# Derive mu  
  mu = beta0 + beta1*x

# Single random realization of y, 
#  based on mu and sigma  
  set.seed(43252)
  y =  rnorm(length(mu),mean = mu, sd = sigma)


## ---- eval=TRUE, echo=FALSE-----------------------
#plot x and y  
  par(cex.axis=1.3,cex.lab=1.3)
  plot(x,y,type="p",col=4,lwd=4,ylim=c(-2,4),
       pch=18,cex=2)
  lines(x,mu,lwd=3)


## ---- eval=TRUE, echo=FALSE-----------------------
#sample many times  
  y.many = replicate(1000,rnorm(length(mu),mu, sigma))
  dim(y.many)


## ---- eval=TRUE, echo=FALSE-----------------------
#plot all samples with true mu
  par(cex.axis=1.3,cex.lab=1.3)
  matplot(y.many,type="p",pch=18,xaxt="n",col=2,cex=2,xlab="x",ylab="y",ylim=c(-2,4))
  axis(1,at=1:length(x),lab=x)
  lines(1:length(x),mu,lwd=3)
  points(1:length(x),y,type="p",col=4,lwd=4,ylim=c(-2,3),
       pch=18,cex=2)


## ---- eval=TRUE, echo=TRUE------------------------
y <- matrix(c(1,2,3),nrow=1,ncol=3)
y


## ---- eval=TRUE, echo=TRUE------------------------
y <- matrix(c(1,2,3),nrow=3,ncol=1)
y


## ---- eval=TRUE, echo=TRUE------------------------
x <- matrix(c(0.5,1,-2),nrow=3,ncol=1)
x


## ---- eval=TRUE, echo=TRUE------------------------
p=3
beta <- matrix(c(0,-2,2),nrow=p,ncol=1)
beta


## ---- eval=TRUE, echo=TRUE------------------------
p=3
X <- matrix(c(1,2,3,4,5,6,7,8,9),nrow=3,ncol=p,byrow=FALSE)
X


## ---- eval=TRUE, echo=TRUE------------------------
t(y)%*%y


## ---- eval=TRUE, echo=TRUE------------------------
first=t(y)
dim(first)


## ---- eval=TRUE, echo=TRUE------------------------
second=y
dim(second)


## ---- eval=TRUE, echo=TRUE------------------------
#When this is true
  ncol(first)==nrow(second)


## ---- eval=TRUE, echo=TRUE------------------------
t(y)%*%y

y%*%t(y)


## ---- eval=TRUE, echo=TRUE------------------------
# GLM coefs
  eleph=read.csv("elephant.study.csv")
  X=model.matrix(~sex+age.years,data=eleph)
  coef(glm(weight~0+X,data=eleph))


## ---- eval=TRUE, echo=TRUE------------------------
# Linear Algebra Coefs
  y=eleph$weight
  c(solve(t(X)%*%X)%*%t(X)%*%y)


## ---- eval=TRUE, echo=FALSE-----------------------
   n=100
   set.seed(43243)
  x=rnorm(n,1,0.6)
  p=0.2+0.3*x+rnorm(n,0,0.05)
  plot(x,p,xlab="Variable",ylab="Probability",
       ylim=c(-0.5,1.5),xlim=c(-2,3),cex=1.5)
    abline(h=c(0,1),lwd=3,col='purple')


## ---- eval=TRUE, echo=FALSE-----------------------
  plot(x,p,xlab="Variable",ylab="Probability",
       ylim=c(-0.5,1.5),xlim=c(-2,3),cex=1.5)
  abline(h=c(0,1),lwd=3,col='purple')

  #abline(glm(p~x),lwd=3,col=2)
  newdata=data.frame(x=seq(-1,3,by=0.25))
  a=predict(glm(p~x),newdata = newdata,se.fit = TRUE)
  LCL=a$fit-a$se.fit*qnorm(0.025)
  UCL=a$fit-a$se.fit*qnorm(0.975)
  lines(newdata$x,a$fit,lwd=3,col=2)
  lines(newdata$x,LCL,lwd=3,col=3)
  lines(newdata$x,UCL,lwd=3,col=3)


## ---- eval=TRUE,echo=TRUE-------------------------
#Design matrix
  set.seed(6454)
  Var1 = seq(0,20,by=1)+rnorm(21,0,2)
  X = model.matrix(~Var1)
  head(X)
  


## ---- eval=TRUE,echo=TRUE-------------------------
#Marginal Coefficients  
  beta=matrix(c(0,5))

#linear terms
  lt = X%*%beta

#mu (link function)
  mu=lt*1

# Plot relationship b/w mean (mu) and variable of interest
  plot(X[,2],mu,type="l",lwd=4)  


## ---- eval=TRUE,echo=TRUE-------------------------
#sample
  set.seed(5435)
  y = rnorm(length(mu),mu,sd=3)
  y


## ---- eval=TRUE,echo=TRUE-------------------------
# Plot relationship b/w mean (mu) and variable of interest
  plot(X[,2],mu,type="l",lwd=4)  
  points(X[,2],y,pch=18,col=2,cex=2)


## ---- eval=TRUE, echo=TRUE------------------------
# Fit model to sample
  model1=glm(y~0+X,family = gaussian(link = "identity"))


## ---- eval=TRUE, echo=TRUE------------------------
  summary(model1)


## ----echo = TRUE----------------------------------
library(equatiomatic)
extract_eq(model1)


## ---- eval=TRUE, echo=TRUE------------------------
 #sample many times  
  y.many = replicate(1000,rnorm(length(mu),mu, sigma))
  dim(y.many)
  
 #Estimate coefs for all 100 samples
  coef.est=apply(y.many,2,FUN=function(y){
              model1=glm(y~0+X,family = gaussian(link = "identity"))
              model1$coefficients
  })

  dim(coef.est)


## ---- eval=TRUE, echo=FALSE-----------------------
  plot(density(coef.est[1,],adjust=2),type="l",lwd=2,
       main=bquote("Sampling Distribution"~beta[0]),
       xlab=bquote(beta[0]))
      abline(v=beta[1],col=2,lwd=4)
      legend("topright",lwd=3,col=2,legend="True Value")
  plot(density(coef.est[2,],adjust=2),type="l",lwd=2,
       main=bquote("Sampling Distribution"~beta[1]),
       xlab=bquote(beta[1]))
  abline(v=beta[2],col=2,lwd=4)
    legend("topright",lwd=3,col=2,legend="True Value")


## ---- eval=TRUE,echo=TRUE, fig.align='center'-----
p=seq(0.001,0.999,by=0.01)
logit.p=qlogis(p)
par(cex.lab=1.5,cex.axis=1.5)
plot(p,logit.p,type="l",lwd=4,col=3,xlab='p',ylab="logit(p)")



## ---- eval=TRUE,echo=TRUE, fig.align='center'-----
#Design matrix
  set.seed(43534)
  Var1 = rnorm(100)
  X = model.matrix(~Var1)
  head(X)


## ---- eval=TRUE,echo=TRUE, fig.align='center'-----
# marginal coefficients (on logit-scale)
  beta=c(-2,4)

#linear terms
  lt = X%*%beta

#transformation via link function to probability scale
  p=plogis(lt)
  head(round(p,digits=2))

#sample
  set.seed(14353)
  y = rbinom(n=length(p),size=1,p)
  y


## ---- eval=TRUE,echo=TRUE, fig.align='center'-----
#Plot the linear 'terms'  and explantory variable
  par(cex.lab=1.2,cex.axis=1.2)
  plot(Var1,lt,type="b",lwd=3,col=2,
       xlab="x",ylab="Linear Terms (logit-value)")  

  index=order(Var1)
  plot(Var1[index],p[index],type="b",lwd=3,col=2,xlab="x",ylab="Probability")  


## ---- eval=TRUE, echo=TRUE------------------------
# Fit model to sample 
  model1=glm(y~0+X, family = binomial(link = "logit"))
  summary(model1)


## ---- eval=TRUE, echo=TRUE------------------------
 #sample many times  
  n.sim=1000
  y.many = replicate(n.sim,rbinom(n=length(p),size=1,p))
  dim(y.many)
  
 #Estimate coefs for all 100 samples
  coef.est=apply(y.many,2,FUN=function(y){
          model1=glm(y~0+X, family = binomial(link = "logit"))
          
  model1$coefficients
  })

  dim(coef.est)


## ---- eval=TRUE, echo=FALSE-----------------------
  plot(density(coef.est[1,],adjust=2),type="l",lwd=2,
       main=bquote("Sampling Distribution"~beta[0]),
       xlab=bquote(beta[0]))
      abline(v=beta[1],col=2,lwd=4)
            legend("topright",lwd=3,col=2,legend="True Value")


## ---- eval=TRUE, echo=FALSE-----------------------
  plot(density(coef.est[2,],adjust=2),type="l",lwd=2,
       main=bquote("Sampling Distribution"~beta[1]),
       xlab=bquote(beta[0]))
  abline(v=beta[2],col=2,lwd=4)
        legend("topright",lwd=3,col=2,legend="True Value")


## ---- eval=TRUE, echo=TRUE------------------------
# Relative Bias
  (median(coef.est[1,])-beta[1])/beta[1]
  (median(coef.est[2,])-beta[2])/beta[2]


## ---- eval=TRUE, echo=TRUE------------------------
# Probability of estimating a coef of the wrong sign
  length(which(coef.est[1,]>0))/n.sim
  length(which(coef.est[2,]<0))/n.sim


## ---- eval=TRUE, echo=TRUE------------------------
# Probability of estimating the slope 2x the truth
    length(which(coef.est[2,]>2*beta[2]))/n.sim


## ---- eval=TRUE, echo=TRUE------------------------
# Probability of estimating the slope within 1
length(which(coef.est[2,]>=beta[2]-1 & coef.est[2,]<=beta[2]+1))/n.sim


## ----eval=TRUE, echo=TRUE-------------------------
library(faux)
n=100
set.seed(543531)
x.var <- rnorm_multi(n = n, 
                  mu = c(10, 20),
                  sd = c(1, 1),
                  r = c(0), 
                  varnames = c("A", "B"),
                  empirical = FALSE)

set.seed(54353)
x.var.cor <- rnorm_multi(n = n, 
                  mu = c(10, 20),
                  sd = c(1, 1),
                  r = c(0.8), 
                  varnames = c("A", "B"),
                  empirical = FALSE)

#Correlation
  cor(x.var)
  cor(x.var.cor)



## ----eval=TRUE, echo=TRUE, fig.align='center'-----
par(mfrow=c(1,2))
  plot(x.var.cor$A,x.var.cor$B)
  plot(x.var$A,x.var$B)


## ----eval=TRUE, echo=TRUE-------------------------

#Design matrices
  X.cor=model.matrix(~x.var.cor$A+x.var.cor$B)
  X=model.matrix(~x.var$A+x.var$B)

#True coefs  
  beta=c(1,2,3)

#Derive mu  
  mu=X%*%beta
  mu.cor=X.cor%*%beta

#simulate data  
  set.seed(54353)
  y=rnorm(n,mu,2)
  y.cor=rnorm(n,mu.cor,2)


## ----eval=TRUE, echo=TRUE-------------------------
summary(glm(y~0+X))


## ----eval=TRUE, echo=TRUE-------------------------
summary(glm(y~0+X.cor))

