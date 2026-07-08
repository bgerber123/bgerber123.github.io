## ----eval=TRUE,echo=FALSE,fig.align='center'---------------------
set.seed(923874)                 # Create example data

data <- data.frame(Site = c("Site 1","Site 2"),
                         Pop.Size = c(75,100),
                         lowerCI = c(40,80),
                         upperCI = c(90,120))


library("ggplot2")

ggplot(data, aes(Site, Pop.Size)) +        # ggplot2 plot with confidence intervals
  geom_point(cex=4) +
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI),size = 1)

knitr::kable(head(data))



## ----eval=TRUE,echo=FALSE----------------------------------------
set.seed(5435)
rnorm(10,75,20)


## ----eval=TRUE,echo=FALSE,fig.align='center'---------------------
par(mfrow=c(1,1))
set.seed(54354)
post1=rnorm(10000,75,20)
hist(post1,xlim=c(0,200),freq=FALSE,main="Posterior Probability Distribution",xlab="Population Size at Site 1")
curve(dnorm(x,75,20),xlim=c(0,200),main="Posterior Probability Distribution",ylab="Density",lwd=3,
      xlab="Population Size at Site 1",add=TRUE)


## ----eval=TRUE,echo=FALSE----------------------------------------
set.seed(5435)
rnorm(10,75,20)


## ----eval=TRUE,echo=FALSE,fig.align='center'---------------------
par(mfrow=c(1,1))
hist(post1,xlim=c(0,200),freq=FALSE,main="Posterior Probability Distribution",xlab="Population Size at Site 1")
abline(v=mean(post1),lwd=3,col=2)
abline(v=quantile(post1,probs=c(0.025,0.975)),lwd=3,col=4)
curve(dnorm(x,75,20),xlim=c(0,200),main="Posterior Probability Distribution",ylab="Density",lwd=3,xlab="Population Size at Site 1")
abline(v=mean(post1),lwd=3,col=2)
abline(v=quantile(post1,probs=c(0.025,0.975)),lwd=3,col=4)



## ----eval=TRUE,echo=FALSE, fig.align='center'--------------------
par(mfrow=c(1,1))
set.seed(4345)
post1=post1
post2=rnorm(10000,100,15)
hist(post1,freq=FALSE,xlim=c(20,150),ylim=c(0,0.025),main="Posterior Distributions \nof Adundance for Site 1 and Site 2 ",
     xlab="Population Size")
hist(post2,freq=FALSE,xlim=c(20,150),add=TRUE,col=2)



## ----eval=TRUE,echo=TRUE-----------------------------------------
diff = post2-post1


## ----eval=TRUE,echo=FALSE, fig.align='center'--------------------
diff=post2-post1
hist(diff,freq=FALSE,col=3,main="Posterior Distribution \nof the difference in Adundance \nfor Site 1 and Site 2 ",
     xlab="Pop Size 2 - Pop Size 1")
abline(v=0, lwd=3,lty=3)


## ----eval=TRUE,echo=TRUE-----------------------------------------
length(which(diff>0))/length(diff)


## ----eval=TRUE,echo=FALSE----------------------------------------
set.seed(5435)
elev = seq(-1,1,by=0.1)
b.size =rnorm(21,mean=1+1*elev,sd=1)


## ----eval=TRUE,echo=TRUE-----------------------------------------
summary(glm(b.size~elev))


## ----eval=TRUE,echo=FALSE----------------------------------------
set.seed(543534)
post=rnorm(1000,0.5,1)
hist(post,main="Posterior Distribution of Effect of Elevation on Beetle Size",freq = FALSE,xlab="Slope/Coeficient")
abline(v=0, lwd=4,col=2)
abline(v=mean(post), lwd=4,col=3)
legend("topleft",col=c(2,3),lwd=4,legend=c("No Effect", "Posterior Mean"))


## ----eval=TRUE,echo=TRUE-----------------------------------------
  mean(post)


## ----eval=TRUE,echo=TRUE-----------------------------------------
  quantile(post,prob=0.5)


## ----eval=TRUE,echo=TRUE-----------------------------------------
  modeest::shorth(post)


## ----eval=TRUE,echo=TRUE-----------------------------------------
# Quantile Credible Intervals:
  quantile(post,
           prob = c(0.025,0.975)
           )


## ----eval=TRUE,echo=TRUE-----------------------------------------
# Highest Posterior Density Intervals (HPDI)
  coda::HPDinterval(coda::as.mcmc(post),
                    prob = 0.95
                    )
     


## ----eval=TRUE,echo=TRUE-----------------------------------------
# Probability of a positive effect (exact p-value)
 length(which(post>0))/length(post)


## ----eval=TRUE,echo=TRUE-----------------------------------------
# Probability of a negative effect (exact p-value)
 length(which(post<0))/length(post)


## ----------------------------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

# # P(A)
# 3/10 = 0.3
# 
# # P(B)
# 5/10 = 0.5


## ----------------------------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

# # P(A and B)
# 2/10 = 0.2
# 


## ----------------------------------------------------------------
#| echo: TRUE
#| eval: FALSE
#| code-fold: true
#| code-summary: "Click for Answer"

# # P(A|B)
# 2/5 = 0.4
# 
# # P(B|A)
# 2/3 = 0.6666


## ----------------------------------------------------------------
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


## ----prior, echo=TRUE, eval=TRUE---------------------------------
curve(dbeta(x, 4,2),xlim=c(0,1),lwd=5)


## ----prior2, echo=TRUE, eval=TRUE--------------------------------
curve(dbeta(x, 1,1),xlim=c(0,1),lwd=5)


## ----eval=TRUE, echo=TRUE----------------------------------------
curve(dbeta(x,1,1),xlim=c(0,1),lwd=3,col=2,xlab="p",
      ylab = "Prior Probability Density")


## ----eval=TRUE, echo=TRUE----------------------------------------
# Survival outcomes of three adult hippos
  y1 = c(0,0,0,0,0,0,0,1,1)
  N1 = length(y1)
  mle.p = mean(y1)
  mle.p


## ----eval=TRUE, echo=TRUE----------------------------------------
  alpha.prior1 = 1
  beta.prior1 = 1


## ----eval=TRUE, echo=FALSE---------------------------------------
#Plot of prior 1
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,20))
legend("topleft",col=c(1,2),legend=c("Prior 1"),lwd=3)


## ----eval=TRUE, echo=TRUE----------------------------------------
  alpha.prior2 = 10
  beta.prior2 = 2


## ----eval=TRUE, echo=FALSE---------------------------------------
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,20))
legend("topleft",col=c(1,2),legend=c("Prior 1"),lwd=3)
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),lwd=3,col=2,add=TRUE)
legend("topleft",col=c(1,2),legend=c("Prior 1", "Prior 2"),lwd=3)


## ----eval=TRUE, echo=TRUE----------------------------------------
  alpha.prior3 = 150
  beta.prior3 = 15


## ----eval=TRUE, echo=FALSE---------------------------------------
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),lwd=3,
      xlab="Probability",ylab="Probabilty Density",
      main="Prior Probability of Success",ylim=c(0,20))
legend("topleft",col=c(1,2),legend=c("Prior 1"),lwd=3)
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),lwd=3,col=2,add=TRUE)
curve(dbeta(x,shape1=alpha.prior3,shape2=beta.prior3),lwd=3,col=3,add=TRUE)
legend("topleft",col=c(1,2,3),legend=c("Prior 1", "Prior 2","Prior 3"),lwd=3)



## ----eval=TRUE, echo=TRUE----------------------------------------
# Note how the data are the same, but the prior is changing.
# Gibbs sampler
  post.1 = rbeta(10000,alpha.prior1+sum(y1),beta.prior1+N1-sum(y1))
  post.2 = rbeta(10000,alpha.prior2+sum(y1),beta.prior2+N1-sum(y1))
  post.3 = rbeta(10000,alpha.prior3+sum(y1),beta.prior3+N1-sum(y1))


## ----eval=TRUE, echo=FALSE---------------------------------------
plot(density(post.1,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=1,lwd=3,main="Prior 1",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior1,shape2=beta.prior1),
      add=TRUE,col=1,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topright",lwd=3,lty=c(1,3,1),col=c("black","black","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE---------------------------------------
plot(density(post.2,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=2,lwd=3,main="Prior 2",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),
      add=TRUE,col=2,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topright",lwd=3,lty=c(1,3,1),col=c("red","red","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE---------------------------------------
plot(density(post.3,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=3,lwd=3,main="Prior 3",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior3,shape2=beta.prior3),
      add=TRUE,col=3,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("green","green","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=TRUE----------------------------------------
y2 = c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
length(y2)


## ----eval=TRUE, echo=FALSE---------------------------------------
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


## ----eval=TRUE, echo=FALSE---------------------------------------
plot(density(post.5,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=2,lwd=3,main="Prior 2",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior2,shape2=beta.prior2),
      add=TRUE,col=2,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("red","red","purple"),
       legend=c("Posterior","Prior","MLE"))


## ----eval=TRUE, echo=FALSE---------------------------------------
plot(density(post.6,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=3,lwd=3,main="Prior 3",
     xlab="Posterior Probability",ylab="Probability Density")
curve(dbeta(x,shape1=alpha.prior3,shape2=beta.prior3),
      add=TRUE,col=3,lwd=3,lty=3)
abline(v=mle.p,col="purple",lwd=3)
legend("topleft",lwd=3,lty=c(1,3,1),col=c("green","green","purple"),
       legend=c("Prior 1","Prior 2","Prior 3"))



## ----eval=TRUE, echo=FALSE---------------------------------------
par(mfrow=c(1,2))
plot(density(post.1,adjust=1.3),ylim=c(0,20),xlim=c(0,1),col=1,lwd=3,main="Less Data",
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



## ----eval=TRUE,echo=FALSE----------------------------------------
set.seed(34534)
x =rnorm(500,0,4)
plot(1:500,x,type="b",ylab="Parameter Value", xlab = "MCMC Sample Iteration",pch=19 )


## ----eval=TRUE,echo=FALSE----------------------------------------
plot(1:500,x,type="l",ylab="Parameter Value", xlab = "MCMC Sample Iteration")
legend("top", 
       legend = c("Red = Discard", "Black = Keep"), 
       col = c("red", "black"), 
       pch = 16, 
       horiz = TRUE)
my_colors <- rep(c("red", "black"),250)
points(1:500, x, col = my_colors, pch = 16, cex = 0.5)


## ----eval=TRUE,echo=FALSE----------------------------------------
set.seed(34534)
x2=rnorm(500,-10+0.02*(1:500),3)
plot(1:1000,c(x2,x),type="l",ylab="Parameter Value", xlab = "MCMC Sample Iteration")
legend("top", 
       legend = c("Red = Discard", "Black = Keep"), 
       col = c("red", "black"), 
       pch = 16, 
       horiz = TRUE)
my_colors <- c(rep("red", 500), rep("black",250))
points(1:500, x2, col = my_colors, pch = 16, cex = 0.5)


## ----eval=TRUE,echo=FALSE----------------------------------------
set.seed(34534)
x.chain1 =rnorm(500,0,6)
x.chain2 =rnorm(500,0,6)
x.chain3 =rnorm(500,0,6)

x2.chain1=rnorm(500,-10+0.02*(1:500),5)
x2.chain2=rnorm(500,10-0.02*(1:500),5)
x2.chain3=rnorm(500,0,5)




plot(1:1000,c(x2.chain1,x.chain1),type="l",ylab="Parameter Value", xlab = "MCMC Sample Iteration" )
lines(1:1000,c(x2.chain2,x.chain2),col=2)
lines(1:1000,c(x2.chain3,x.chain3),col=3)

legend("top", 
       legend = c("Red = Chain1", "Black = Chain2", "Green = Chain3"), 
       col = c("red", "black","green"), 
       pch = 16, 
       horiz = TRUE)


