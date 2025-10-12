## ----eval=TRUE,echo=FALSE--------------------
set.seed(452)
y=rpois(1000,10)
main="Distribution of counts of plants \nin all possible plots"


## ----eval=TRUE,echo=TRUE---------------------
#| code-line-numbers: 2,3
par(mfrow=c(1,2))
hist(y, breaks=20,xlim=c(0,25),main=main)
hist(y, breaks=20,xlim=c(0,25),freq = FALSE,main=main)


## ----eval=TRUE,echo=FALSE--------------------
par(mfrow=c(1,2))
set.seed(452)
y=rpois(50,10)
hist(y, main="Sample of counts of plants (n=50)",breaks=10,xlim=c(0,25))
hist(y, main="Sample of counts of plants (n=50)",breaks=10,xlim=c(0,25),freq = FALSE)


## ----bern, echo=TRUE, eval=TRUE--------------
#Define inputs
  theta=0.2;  N=1 

#Random sample - 1 duck
  rbinom(n=1,size=N,theta)

#Random sample - 10 ducks
  rbinom(n=10,size=N,theta)


## ----bern2, echo=TRUE, eval=TRUE-------------
y.mat = replicate(1000,rbinom(n = 10,size=N,theta))
theta.hat = apply(y.mat, 2, mean)


## ----bern3, echo=FALSE, eval=TRUE,  out.width="75%"----
hist(theta.hat,freq=TRUE,breaks=40, main=bquote("Sampling Distribution of"~theta),xlab=expression(theta))


## ----eval=TRUE, echo=TRUE--------------------
# 1 duck tagged/released and one simulation
  theta=0.2;  N=1 
  rbinom(n=1,size=N,theta)


## ----eval=TRUE, echo=TRUE--------------------
# 1000 ducks tagged/released and one simulation
  theta=0.2;  N=1000 
  rbinom(n=1,size=N,theta)


## ----eval=TRUE, echo=TRUE--------------------
# 1000 ducks tagged/released and 10 simulation
  theta=0.2;  N=1000 
  rbinom(n=10,size=N,theta)


## ----eval=TRUE, echo=TRUE--------------------
# 1 duck tagged for each of 1000 simulations
  theta=0.2;  N=1
  y = rbinom(n=1000,size=N,theta)
  y


## ----eval=TRUE, echo=TRUE--------------------
sum(y)


## ----eval=TRUE,echo=FALSE--------------------
shape=100
rate=1
scale=1/rate

set.seed(4454)
round(rgamma(10,shape,rate),digits=2)



## ----echo=FALSE,eval=TRUE--------------------
mu=shape*scale
var=shape*scale^2
curve(dgamma(x, shape=shape,rate=rate),xlim=c(0,200),lwd=4,xlab="y",
      ylab="dgamma(y,shape = 100, rate = 1",main="Equivalent means and variances")
curve(dnorm(x, mu,sqrt(var)),add=TRUE,col=2,lwd=4,lty=2)
legend("topright",legend = c("Gamma PDF (shape = 100, rate = 1)", "Normal PDF (mu = 100, var = 100)"),lwd=3,col=c(1,2))


## ----echo=FALSE,eval=TRUE--------------------
shape=1
rate=1
scale=1/rate

mu=shape*scale
var=shape*scale^2
curve(dgamma(x, shape=shape,rate=rate),xlim=c(-5,5),lwd=4,xlab="y",
      ylab="dgamma(y,shape = 1, rate = 1",main="Equivalent means and variances")
curve(dnorm(x, mu,sqrt(var)),add=TRUE,col=2,lwd=4,lty=2)
legend("topright",legend = c("Gamma PDF (shape = 1, rate = 1)", "Normal PDF (mu = 1, var = 1)"),lwd=3,col=c(1,2))


## ----echo=FALSE,eval=TRUE--------------------
library(visualize)


## ----echo=TRUE,eval=TRUE---------------------
visualize.it(dist = 'norm', stat = c(100),
             list(mu = 100 , sd = 10), section = "upper")


## ----echo=FALSE,eval=TRUE--------------------
library(visualize)
visualize.it(dist = 'norm', stat = c(120),
             list(mu = 100 , sd = 10), section = "upper")


## ----echo=TRUE-------------------------------
pnorm(120,mean=100,sd=10,lower.tail = FALSE)


## ----echo=TRUE-------------------------------
qnorm(0.02275,100,10,lower.tail = FALSE)


## ----pdf1, echo=TRUE, eval=TRUE--------------
y = rnorm(1000, mean = 20, sd = 3)
hist(y,freq=FALSE,ylim=c(0,0.14))
lines(density(y),lwd=3,col=4)


## ----pdf1b, echo=TRUE, eval=TRUE-------------
curve(dnorm(x, mean= 20, sd = 3),
      xlim=c(0,40),lwd=3,col=2,ylab="Probability Density",xlab="y")
abline(v=20, lwd=3, col=1, lty=4)


## ----pdf2a, echo=TRUE, eval=FALSE------------
# curve(dnorm(x, mean = 10, sd = 3),xlim=c(0,40),lwd=4,col=3,add=TRUE)


## ----pdf2b, echo=FALSE, eval=TRUE------------
curve(dnorm(x, mean = 20, sd = 3),xlim=c(0,40),lwd=3,col=2,ylab="Probability Density",xlab="y")
abline(v=20, lwd=3, col=1, lty=4)
curve(dnorm(x, mean= 10, sd = 3),xlim=c(0,40),lwd=4,col=3,add=TRUE)


## ----pdf3, echo=TRUE, eval=TRUE--------------
shape =10
scale = 2

mean1 = shape*scale
mean1

mode1 = (shape-1)*scale
mode1

stdev = sqrt(shape*scale^2)
stdev


## ----pdf4, echo=FALSE, eval=TRUE-------------
curve(dgamma(x, shape = shape, scale=scale),xlim=c(0,50),lwd=3,col=2,ylab="Probability Density",xlab="y")
abline(v=mean1, lwd=3, col=1, lty=4); abline(v=mode1, lwd=3, col=3, lty=4)
legend("topright",lty=3, col=c(1,2),legend=c("Mean","Mode"),lwd=3)


## ----eval=TRUE, echo=TRUE--------------------
pgamma(q=40, shape=10, scale=2,lower.tail=FALSE)


## ----eval=TRUE, echo=TRUE--------------------
pgamma(q=20,shape=10, scale=2,lower.tail=TRUE)


## ----eval=TRUE, echo=TRUE--------------------
pgamma(q=40,shape=10, scale=2,lower.tail=TRUE)-
pgamma(q=20,shape=10, scale=2,lower.tail=TRUE)


## ----eval=TRUE, echo=TRUE--------------------
qgamma(p=0.025,shape=10, scale=2,lower.tail=TRUE)


## ----eval=TRUE, echo=TRUE--------------------
qgamma(p=0.025,shape=10, scale=2,lower.tail=FALSE)


## ----eval=TRUE, echo=TRUE--------------------
curve(dgamma(x,shape=10, scale=2),xlim=c(0,50),lwd=3,
      xlab="y", ylab="dgamma(x,shape=10, scale=2)")
abline(v=c(9.590777,34.16961),lwd=3,col=2)


## ----echo=TRUE, eval=TRUE--------------------
set.seed(154434)
y <- rgamma(100, shape=10, scale=2)


## ----echo=FALSE, eval=TRUE-------------------
curve(dgamma(x,shape=10, scale=2),xlim=c(0,50),lwd=3)
hist(y,col=adjustcolor("red",alpha.f = 0.5),freq=FALSE,add=TRUE,breaks=100)


## ----eval=TRUE,echo=TRUE---------------------
# A data point
  y = c(10)

#the likelihood the mean is 8, given our data
  dnorm(y, mean = 8)


## ----eval=TRUE,echo=TRUE---------------------
#the likelihood the mean is 9, given our data
  dnorm(y, mean = 9)


## ----eval=TRUE,echo=TRUE---------------------
  means = seq(0, 20,by = 0.1) # many guesses of the mean
  likelihood = dnorm(y, mean = means, sd = 1) # likelihood of each guess of the mean


## ----eval=TRUE,echo=FALSE, fig.align='center'----
#Look at guesses and likelihood
  plot(means,likelihood,xlab="Guesses for the Mean")
  abline(v=10,lwd=3,col=3)
  legend("topright",legend=c("Max Likelihood"),
         lwd=3,col=3)


## ----echo=TRUE, eval=TRUE--------------------
# penguin height data
  y = c(4.34, 3.53, 3.75)

# Joint likelihood of mu=3, sigma =1, given our data
  prod(dnorm(y, mean = 3,sd = 1))


## ----echo=TRUE, eval=TRUE--------------------
# The Guesses
  mu = seq(0,6,0.05)
  sigma = seq(0.01,2,0.05)
  try = expand.grid(mu,sigma)
  colnames(try) = c("mu","sigma")

# function
fun = function(a,b){
          prod(dnorm(y,mean = a, sd = b))
      }

# mapply the function with the inputs
  likelihood = mapply(a = try$mu, b = try$sigma, FUN=fun)


## ----echo=TRUE, eval=TRUE--------------------
# maximum likelihood of parameters
  try[which.max(likelihood),]


## ----echo=TRUE, eval=TRUE--------------------
sum(y)/length(y)


## ----echo=FALSE,eval=TRUE, out.width="150%"----
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
all=cbind(try,likelihood)
colnames(all)=c("mu","sigma","likelihood")
fig <- plot_ly(all, x = ~mu, y = ~sigma, z = ~likelihood, marker = list(size = 5),width = 800, height = 800)
fig <- fig %>% add_markers(color=~likelihood)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'mu'),
                     yaxis = list(title = 'sigma'),
                     zaxis = list(title = 'Likelihood')))
fig %>% layout(font = f, margin = m)

#fig


## ----eval=TRUE,echo=FALSE--------------------
set.seed(154541)
y=rnorm(100,3.8,1)

try=expand.grid(seq(0,6,0.01),seq(0.01,2,0.01))
  colnames(try)=c("mu","sigma")

# mapply the function with the inputs
  likelihood=mapply(try$mu,try$sigma, FUN=fun)

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
all=cbind(try,likelihood)
colnames(all)=c("mu","sigma","likelihood")
fig <- plot_ly(all, x = ~mu, y = ~sigma, z = ~likelihood, marker = list(size = 5),width = 800, height = 800)
fig <- fig %>% add_markers(color=~likelihood)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Mean'),
                     yaxis = list(title = 'SD'),
                     zaxis = list(title = 'Likelihood')))
fig %>% layout(font = f, margin = m)

  


## ----eval=TRUE,echo=TRUE---------------------
fun.log = function(a,b){
              sum(dnorm(y,mean = a, sd = b, log=TRUE))
          }

log.likelihood = mapply(a = try$mu, b = try$sigma, FUN=fun.log)
  
# maximum log-likelihood of parameters
  try[which.max(log.likelihood),]



## ----echo=TRUE-------------------------------

# Note: optim function uses minimization, not maximization. 
# WE want to find the minimum negative log-likelihood
# THUS, need to put negative in our function

neg.log.likelihood=function(par){
  -sum(dnorm(y,mean=par[1],sd=par[2],log=TRUE))
  }

#find the values that minimizes the function
#c(1,1) are the initial values for mu and sigma
fit <- optim(par=c(1,1), fn=neg.log.likelihood,
             method="L-BFGS-B",
             lower=c(0,0),upper=c(10,1)
             )

#Maximum likelihood estimates for mu and sigma
fit$par


## ----echo=TRUE-------------------------------
out = lm(y~1)
summary(out)


## ----eval=TRUE,echo=FALSE--------------------
set.seed(923874)                 # Create example data
data <- data.frame(Site = c("Site 1","Site 2"),
                         Pop.Size = c(75,100),
                         lower = c(40,80),
                         upper = c(90,120))

library("ggplot2")
ggplot(data, aes(Site, Pop.Size)) +        # ggplot2 plot with confidence intervals
  geom_point(cex=4) +
  geom_errorbar(aes(ymin = lower, ymax = upper),size = 1)



## ----eval=TRUE,echo=FALSE--------------------
set.seed(5435)
rnorm(10,75,20)


## ----eval=TRUE,echo=FALSE,fig.align='center'----
par(mfrow=c(1,2))
set.seed(54354)
post1=rnorm(10000,75,20)
hist(post1,xlim=c(0,200),freq=FALSE,main="Posterior Probability Distribution",xlab="Population Size at Site 1")
curve(dnorm(x,75,20),xlim=c(0,200),main="Posterior Probability Distribution",ylab="Density",lwd=3,xlab="Population Size at Site 1")


## ----eval=TRUE,echo=FALSE--------------------
set.seed(5435)
rnorm(10,75,20)


## ----eval=TRUE,echo=FALSE,fig.align='center'----
par(mfrow=c(1,2))
hist(post1,xlim=c(0,200),freq=FALSE,main="Posterior Probability Distribution",xlab="Population Size at Site 1")
abline(v=mean(post1),lwd=3,col=2)
abline(v=quantile(post1,probs=c(0.025,0.975)),lwd=3,col=4)
curve(dnorm(x,75,20),xlim=c(0,200),main="Posterior Probability Distribution",ylab="Density",lwd=3,xlab="Population Size at Site 1")
abline(v=mean(post1),lwd=3,col=2)
abline(v=quantile(post1,probs=c(0.025,0.975)),lwd=3,col=4)



## ----eval=TRUE,echo=FALSE, fig.align='center'----
par(mfrow=c(1,2))
set.seed(4345)
post1=post1
post2=rnorm(10000,100,15)
hist(post1,freq=FALSE,xlim=c(20,150),ylim=c(0,0.025),main="Posterior Distributions \nof Adundance for Site 1 and Site 2 ",
     xlab="Population Size")
hist(post2,freq=FALSE,xlim=c(20,150),add=TRUE,col=2)
diff=post2-post1
hist(diff,freq=FALSE,col=3,main="Posterior Distributions \nof the difference in Adundance \nfor Site 1 and Site 2 ",
     xlab="Pop Size 2 - Pop Size 1")


## ----eval=TRUE,echo=TRUE---------------------
diff=post2-post1
length(which(diff>0))/length(diff)


## ----eval=TRUE,echo=FALSE--------------------
set.seed(5435)
x=seq(-1,1,by=0.1)
y=rnorm(21,mean=1+1*x,sd=1)


## ----eval=TRUE,echo=TRUE---------------------
summary(glm(y~x))


## ----eval=TRUE,echo=FALSE--------------------
set.seed(543534)
post=rnorm(1000,0.5,1)
hist(post,main="Posterior Distribution of Effect of Elevation",freq = FALSE,xlab="Slope/Coeficient")
abline(v=0, lwd=4,col=2)
abline(v=mean(post), lwd=4,col=3)
legend("topleft",col=c(2,3),lwd=4,legend=c("No Effect", "Posterior Mean"))


## ----eval=TRUE,echo=TRUE---------------------
#Posterior Mean
  mean(post)


## ----eval=TRUE,echo=TRUE---------------------
#Credible/Probability Intervals 
  quantile(post,prob=c(0.025,0.975))


## ----eval=TRUE,echo=TRUE---------------------
# #Probabilty of a postive effect
 length(which(post>0))/length(post)


## ----eval=TRUE,echo=TRUE---------------------
# #Probabilty of a negative effect
 length(which(post<0))/length(post)


## --------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

# #P(A)
# 3/10 = 0.3
# 
# #P(B)
# 5/10 = 0.5


## --------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

# #P(A and B)
# 2/10 = 0.2
# 


## --------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

# #P(A|B)
# 2/5 = 0.4
# 
# #P(B|A)
# 2/3 = 0.6666


## --------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

# # P(A or B)
# # P(A) + P(B) - P(A and B)
# 
# 0.3 + 0.5 - 0.2 = 0.6
# 
# 


## ----prior, echo=TRUE, eval=TRUE-------------
curve(dbeta(x, 4,2),xlim=c(0,1),lwd=5)


## ----prior2, echo=TRUE, eval=TRUE------------
curve(dbeta(x, 1,1),xlim=c(0,1),lwd=5)


## ----eval=TRUE, echo=TRUE--------------------
curve(dbeta(x,1,1),xlim=c(0,1),lwd=3,col=2,xlab="p",
      ylab = "Prior Probability Density")


## ----eval=TRUE, echo=TRUE--------------------
# Survival outcomes of three adult hippos
  y1=c(0,0,0,0,0,0,0,1,1)
  N1=length(y1)
  mle.p=mean(y1)
  mle.p


## ----eval=TRUE, echo=TRUE--------------------
  alpha.prior1=1
  beta.prior1=1


## ----eval=TRUE, echo=FALSE-------------------
#Plot of prior 1
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,20))
legend("topleft",col=c(1,2),legend=c("Prior 1"),lwd=3)


## ----eval=TRUE, echo=TRUE--------------------
  alpha.prior2=10
  beta.prior2=2


## ----eval=TRUE, echo=FALSE-------------------
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,20))
legend("topleft",col=c(1,2),legend=c("Prior 1"),lwd=3)
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),lwd=3,col=2,add=TRUE)
legend("topleft",col=c(1,2),legend=c("Prior 1", "Prior 2"),lwd=3)


## ----eval=TRUE, echo=TRUE--------------------
  alpha.prior3=150
  beta.prior3=15


## ----eval=TRUE, echo=FALSE-------------------
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,20))
legend("topleft",col=c(1,2),legend=c("Prior 1"),lwd=3)
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),lwd=3,col=2,add=TRUE)
curve(dbeta(x,shape1=alpha.prior3,shape2=beta.prior3),lwd=3,col=3,add=TRUE)
legend("topleft",col=c(1,2,3),legend=c("Prior 1", "Prior 2","Prior 3"),lwd=3)



## ----eval=TRUE, echo=TRUE--------------------
# Note- the data are the same, but the prior is changing.
# Gibbs sampler
  post.1=rbeta(10000,alpha.prior1+sum(y1),beta.prior1+N1-sum(y1))
  post.2=rbeta(10000,alpha.prior2+sum(y1),beta.prior2+N1-sum(y1))
  post.3=rbeta(10000,alpha.prior3+sum(y1),beta.prior3+N1-sum(y1))


## ----eval=TRUE, echo=FALSE-------------------
plot(density(post.1,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=1,lwd=3,main="Prior 1",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),
      add=TRUE,col=1,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topright",lwd=3,lty=c(1,3,1),col=c("black","black","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE-------------------
plot(density(post.2,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=2,lwd=3,main="Prior 2",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),
      add=TRUE,col=2,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topright",lwd=3,lty=c(1,3,1),col=c("red","red","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE-------------------
plot(density(post.3,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=3,lwd=3,main="Prior 3",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior3,shape2=beta.prior3),
      add=TRUE,col=3,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("green","green","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=TRUE--------------------
y2=c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
length(y2)


## ----eval=TRUE, echo=FALSE-------------------
N2=length(y2)

mle.p=mean(y2)

post.4=rbeta(10000,alpha.prior1+sum(y2),beta.prior1+N2-sum(y2))
post.5=rbeta(10000,alpha.prior2+sum(y2),beta.prior2+N2-sum(y2))
post.6=rbeta(10000,alpha.prior3+sum(y2),beta.prior3+N2-sum(y2))

plot(density(post.4,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=1,lwd=3,main="Prior 1",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),
      add=TRUE,col=1,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("black","black","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE-------------------
plot(density(post.5,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=2,lwd=3,main="Prior 2",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),
      add=TRUE,col=2,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("red","red","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE-------------------
plot(density(post.6,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=3,lwd=3,main="Prior 3",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior3,shape2=beta.prior3),
      add=TRUE,col=3,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("green","green","purple"),
       legend=c("Prior 1","Prior 2","Prior 3"))



## ----eval=TRUE, echo=FALSE-------------------
par(mfrow=c(1,2))
plot(density(post.1,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=1,lwd=3,main="Small Data",
     xlab="Posterior Probability",ylab="Probability Density")
#curve(dbeta(x,shape1=alpha1,shape2=beta1),
#      add=TRUE,col=1,lwd=3,lty=3)
#abline(v=mle.p,col="purple",lwd=3)

lines(density(post.2,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=2,lwd=3)
#curve(dbeta(x,shape1=alpha2,shape2=beta2),
#      add=TRUE,col=2,lwd=3,lty=3)
#abline(v=mle.p,col="purple",lwd=3)

lines(density(post.3,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=3,lwd=3)
#curve(dbeta(x,shape1=alpha3,shape2=beta3),
#      add=TRUE,col=3,lwd=3,lty=3)
#abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,1,1),col=c("black","red","green"),
       legend=c("Prior 1","Prior 2","Prior 3"))

#################
plot(density(post.4,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=1,lwd=3,main="More Data",
     xlab="Posterior Probability",ylab="Probability Density")
#curve(dbeta(x,shape1=alpha1,shape2=beta1),
#      add=TRUE,col=1,lwd=3,lty=3)
#abline(v=mle.p,col="purple",lwd=3)

lines(density(post.5,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=2,lwd=3)
#curve(dbeta(x,shape1=alpha2,shape2=beta2),
#      add=TRUE,col=2,lwd=3,lty=3)
#abline(v=mle.p,col="purple",lwd=3)

lines(density(post.6),ylim=c(0,20),xlim=c(0,1),col=3,lwd=3)
#curve(dbeta(x,shape1=alpha3,shape2=beta3),
#      add=TRUE,col=3,lwd=3,lty=3)
#abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,1,1),col=c("black","red","green"),
       legend=c("Prior 1","Prior 2","Prior 3"))


