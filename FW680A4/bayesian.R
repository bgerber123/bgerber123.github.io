## ----eval=TRUE,echo=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(923874)                 # Create example data
data <- data.frame(Site = c("Site 1","Site 2"),
                         Pop.Size = c(75,100),
                         lower = c(40,80),
                         upper = c(90,120))

library("ggplot2")
ggplot(data, aes(Site, Pop.Size)) +        # ggplot2 plot with confidence intervals
  geom_point(cex=4) +
  geom_errorbar(aes(ymin = lower, ymax = upper),size = 1)



## ----eval=TRUE,echo=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(5435)
rnorm(10,75,20)


## ----eval=TRUE,echo=FALSE,fig.align='center'-----------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
set.seed(54354)
post1=rnorm(10000,75,20)
hist(post1,xlim=c(0,200),freq=FALSE,main="Posterior Probability Distribution",xlab="Population Size at Site 1")
curve(dnorm(x,75,20),xlim=c(0,200),main="Posterior Probability Distribution",ylab="Density",lwd=3,xlab="Population Size at Site 1")


## ----eval=TRUE,echo=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(5435)
rnorm(10,75,20)


## ----eval=TRUE,echo=FALSE,fig.align='center'-----------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
hist(post1,xlim=c(0,200),freq=FALSE,main="Posterior Probability Distribution",xlab="Population Size at Site 1")
abline(v=mean(post1),lwd=3,col=2)
abline(v=quantile(post1,probs=c(0.025,0.975)),lwd=3,col=4)
curve(dnorm(x,75,20),xlim=c(0,200),main="Posterior Probability Distribution",ylab="Density",lwd=3,xlab="Population Size at Site 1")
abline(v=mean(post1),lwd=3,col=2)
abline(v=quantile(post1,probs=c(0.025,0.975)),lwd=3,col=4)



## ----eval=TRUE,echo=FALSE, fig.align='center'----------------------------------------------------------------------------------------------
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


## ----eval=TRUE,echo=TRUE-------------------------------------------------------------------------------------------------------------------
diff=post2-post1
length(which(diff>0))/length(diff)


## ----eval=TRUE,echo=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(5435)
x=seq(-1,1,by=0.1)
y=rnorm(21,mean=1+1*x,sd=1)


## ----eval=TRUE,echo=TRUE-------------------------------------------------------------------------------------------------------------------
summary(glm(y~x))


## ----eval=TRUE,echo=FALSE------------------------------------------------------------------------------------------------------------------
set.seed(543534)
post=rnorm(1000,0.5,1)
hist(post,main="Posterior Distribution of Effect of Elevation",freq = FALSE,xlab="Slope/Coeficient")
abline(v=0, lwd=4,col=2)
abline(v=mean(post), lwd=4,col=3)
legend("topleft",col=c(2,3),lwd=4,legend=c("No Effect", "Posterior Mean"))


## ----eval=TRUE,echo=TRUE-------------------------------------------------------------------------------------------------------------------
#Posterior Mean
  mean(post)


## ----eval=TRUE,echo=TRUE-------------------------------------------------------------------------------------------------------------------
#Credible/Probability Intervals 
  quantile(post,prob=c(0.025,0.975))


## ----eval=TRUE,echo=TRUE-------------------------------------------------------------------------------------------------------------------
# #Probabilty of a postive effect
 length(which(post>0))/length(post)


## ----eval=TRUE,echo=TRUE-------------------------------------------------------------------------------------------------------------------
# #Probabilty of a negative effect
 length(which(post<0))/length(post)


## ------------------------------------------------------------------------------------------------------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

## #P(A)
## 3/10 = 0.3
## 
## #P(B)
## 5/10 = 0.5


## ------------------------------------------------------------------------------------------------------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

## #P(A and B)
## 2/10 = 0.2
## 


## ------------------------------------------------------------------------------------------------------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

## #P(A|B)
## 2/5 = 0.4
## 
## #P(B|A)
## 2/3 = 0.6666


## ------------------------------------------------------------------------------------------------------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

## # P(A or B)
## # P(A) + P(B) - P(A and B)
## 
## 0.3 + 0.5 - 0.2 = 0.6
## 
## 


## ----prior, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------
curve(dbeta(x, 4,2),xlim=c(0,1),lwd=5)


## ----prior2, echo=TRUE, eval=TRUE----------------------------------------------------------------------------------------------------------
curve(dbeta(x, 1,1),xlim=c(0,1),lwd=5)


## ----eval=TRUE, echo=TRUE------------------------------------------------------------------------------------------------------------------
curve(dbeta(x,1,1),xlim=c(0,1),lwd=3,col=2,xlab="p",
      ylab = "Prior Probability Density")


## ----eval=TRUE, echo=TRUE------------------------------------------------------------------------------------------------------------------
# Survival outcomes of three adult hippos
  y1=c(0,0,0,0,0,0,0,1,1)
  N1=length(y1)
  mle.p=mean(y1)
  mle.p


## ----eval=TRUE, echo=TRUE------------------------------------------------------------------------------------------------------------------
  alpha.prior1=1
  beta.prior1=1


## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
#Plot of prior 1
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,20))
legend("topleft",col=c(1,2),legend=c("Prior 1"),lwd=3)


## ----eval=TRUE, echo=TRUE------------------------------------------------------------------------------------------------------------------
  alpha.prior2=10
  beta.prior2=2


## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,20))
legend("topleft",col=c(1,2),legend=c("Prior 1"),lwd=3)
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),lwd=3,col=2,add=TRUE)
legend("topleft",col=c(1,2),legend=c("Prior 1", "Prior 2"),lwd=3)


## ----eval=TRUE, echo=TRUE------------------------------------------------------------------------------------------------------------------
  alpha.prior3=150
  beta.prior3=15


## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,20))
legend("topleft",col=c(1,2),legend=c("Prior 1"),lwd=3)
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),lwd=3,col=2,add=TRUE)
curve(dbeta(x,shape1=alpha.prior3,shape2=beta.prior3),lwd=3,col=3,add=TRUE)
legend("topleft",col=c(1,2,3),legend=c("Prior 1", "Prior 2","Prior 3"),lwd=3)



## ----eval=TRUE, echo=TRUE------------------------------------------------------------------------------------------------------------------
# Note- the data are the same, but the prior is changing.
# Gibbs sampler
  post.1=rbeta(10000,alpha.prior1+sum(y1),beta.prior1+N1-sum(y1))
  post.2=rbeta(10000,alpha.prior2+sum(y1),beta.prior2+N1-sum(y1))
  post.3=rbeta(10000,alpha.prior3+sum(y1),beta.prior3+N1-sum(y1))


## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
plot(density(post.1,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=1,lwd=3,main="Prior 1",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),
      add=TRUE,col=1,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topright",lwd=3,lty=c(1,3,1),col=c("black","black","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
plot(density(post.2,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=2,lwd=3,main="Prior 2",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),
      add=TRUE,col=2,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topright",lwd=3,lty=c(1,3,1),col=c("red","red","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
plot(density(post.3,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=3,lwd=3,main="Prior 3",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior3,shape2=beta.prior3),
      add=TRUE,col=3,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("green","green","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=TRUE------------------------------------------------------------------------------------------------------------------
y2=c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
length(y2)


## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
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


## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
plot(density(post.5,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=2,lwd=3,main="Prior 2",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),
      add=TRUE,col=2,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("red","red","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
plot(density(post.6,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=3,lwd=3,main="Prior 3",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior3,shape2=beta.prior3),
      add=TRUE,col=3,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("green","green","purple"),
       legend=c("Prior 1","Prior 2","Prior 3"))



## ----eval=TRUE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
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


