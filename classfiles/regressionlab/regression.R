## ----echo=FALSE,eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
  n = 1000
  b0 = 10
  b1 = 2
  x = rnorm(n,40,10)
  mu = b0+b1*x
  y = rnorm(n,mu,sd=30)
  
  plot(x,y,xlim=c(0,70))


## ----echo=FALSE,eval=TRUE, fig.align='center',width="1000"------------------------------------------------------------------------------------------------
#" (beta0) = 9.06 \n Slope of x (beta1) = 2.0")
plot(x,y,cex.lab=1.3,cex.axis=1.3, main = expression(paste("Intercept (",hat(beta[0]),") = 9.06, Slope of x (",hat(beta[1]),") = 2.0")),xlim=c(0,80))

lm.out=lm(y~x)
abline(lm.out,lwd=3,col=3)
text(x=0,y=9.06,expression(paste(beta[0])),cex=2)
legend("topleft",col=c("green"),lty=2,legend=c("Mean"),lwd=4)



## ----echo=FALSE,eval=TRUE, fig.align='center',width="1000"------------------------------------------------------------------------------------------------
plot(x,y,cex.lab=1.3,cex.axis=1.3, main = expression(paste("Intercept (",hat(beta[0]),") = 9.06, Slope of x (",hat(beta[1]),") = 2.0")),xlim=c(0,80))
lm.out=lm(y~x)

lines(x,predict(lm.out),lwd=3,col=3)

newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence", level = 0.95)

matlines(newx, conf_interval[,2:3], col = "blue", lty=2,lwd=4)

legend("topleft",col=c("green","blue"),lty=2,legend=c("Mean","95% Confidence Interaval (mean)"),lwd=4)



## ----echo=FALSE,eval=TRUE, fig.align='center',width="1000"------------------------------------------------------------------------------------------------
plot(x,y,cex.lab=1.3,cex.axis=1.3, main = expression(paste("Intercept (",hat(beta[0]),") = 9.06, Slope of x (",hat(beta[1]),") = 2.0")),xlim=c(0,80))
lm.out=lm(y~x)

lines(x,predict(lm.out),lwd=3,col=3)

newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence", level = 0.95)

pred_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="prediction", level = 0.95)


matlines(newx, conf_interval[,2:3], col = "blue", lty=2,lwd=4)

matlines(newx, pred_interval[,2:3], col = "purple", lty=2,lwd=4)

legend("topleft",col=c("green","blue","purple"),lty=2,legend=c("Mean","95% Confidence Interaval (mean)", "95% Prediction Interval (data)"),lwd=4)



## ---------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(ggthemes)
m1=broom::augment(lm.out)

ggplot(m1, aes(x, y)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE,linewidth=2) +
  geom_segment(aes(xend = x, yend = .fitted), color = "red", size = 0.3)+theme_base()



## ----eval=TRUE, echo=FALSE,fig.align='center'-------------------------------------------------------------------------------------------------------------
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
  
#plotting
  par(cex.axis=1.3,cex.lab=1.3)
  plot(x,y,type="p",col=4,lwd=4,ylim=c(-2,4),
       pch=18,cex=2)
  lines(x,mu,lwd=3)



## ----eval=TRUE, echo=FALSE, cache=TRUE--------------------------------------------------------------------------------------------------------------------
#sample many times  
  y.many = replicate(1000,rnorm(length(mu),mu, sigma))
#  dim(y.many)


## ----eval=TRUE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
#plot all samples with true mu
  par(cex.axis=1.3,cex.lab=1.3)
  matplot(y.many,type="p",pch=18,xaxt="n",col=2,cex=2,xlab="x",ylab="y",ylim=c(-2,4))
  axis(1,at=1:length(x),lab=x)
  lines(1:length(x),mu,lwd=3)
  points(1:length(x),y,type="p",col=4,lwd=4,ylim=c(-2,3),
       pch=18,cex=2)
  legend("bottomright",lwd=2, col=c(1,4,2),legend=c("Truth","Sample","Sampling Distributions"))


## ----eval=TRUE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
temp.x=x
preds.many = apply(y.many,2,FUN=function(x){
                    predict(lm(x~temp.x)
                   )
})

matplot(preds.many,type="l",xaxt="n",col=grDevices::adjustcolor("black",alpha.f = 0.05),
        lwd=3,
        ylim=c(0,3),xlab="x",ylab=expression(paste(hat(mu))))
        axis(1,at=1:length(x))
lines(1:length(x),mu,type="l",lwd=4,col=2)
legend("bottomright",lwd=3, col=c(2,grDevices::adjustcolor("black",alpha.f = 0.05)),legend=c("Truth", "Estimate"))



## ----eval=TRUE,echo=FALSE, width = 200--------------------------------------------------------------------------------------------------------------------

n = 1000
b0 = 10
b1 = 2
x = rnorm(n,40,10)
mu = b0+b1*x
y = rnorm(n,mu,sd=30)

plot(x,y,xlim=c(0,70))



## ----eval=TRUE,echo=FALSE,fig.align='center'--------------------------------------------------------------------------------------------------------------
n = 1000
b0 = 10
b1 = 20
x = rnorm(n,40,10)
mu = b0+b1*x

sigma =5*x

y = rnorm(n,mu,sd=sigma)

plot(x,y,xlim=c(0,70))



## ----eval=TRUE,echo=TRUE,fig.align='center'---------------------------------------------------------------------------------------------------------------
#Setup parameters
  n = 100 # sample size
  beta0 = 10 # true mean
  sigma = 2 # true std.dev

# Simulate a data set of observations
  set.seed(43243)
  y = rnorm(n, mean = beta0, sd = sigma)


## ----eval=TRUE,echo=TRUE,fig.align='center'---------------------------------------------------------------------------------------------------------------
  hist(y)


## ----eval=TRUE,echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
# Fit model/hypothesis using maximum likelihood
  model1.0 = lm(y~1)

  model1.1 = glm(y~1)

  model1.2 = glm(y~1, family=gaussian(link = identity))

  
  
# Compare Results  
  data.frame(intercept=c(model1.0$coefficients,model1.1$coefficients, model1.2$coefficients),
             SE = c(summary(model1.0)$coefficients[, 2], summary(model1.1)$coefficients[, 2],summary(model1.2)$coefficients[, 2])
            )


## ----eval=TRUE,echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
    
# Summary of model results  
  summary(model1.0)


## ----eval=TRUE,echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
#Predict response for all data
  preds = predict(model1.0, se.fit = TRUE)
  preds


## ----eval=TRUE,echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
# Get 90% confidence intervals (Type I error = 0.1)
  c(
    (preds$fit+preds$se.fit*qnorm(0.05))[1],
    (preds$fit+preds$se.fit*qnorm(0.95))[1]
   )


## ----eval=TRUE,echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
  CI.Normal=confint(model1.0, level=0.9)
  CI.Normal


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------

# Setup
  nboot <- 1000 # number of bootstrap samples
  nobs <- length(y)
  bootcoefs <- rep(NA, nboot)
# Start loop  
for(i in 1:nboot){
  set.seed(43243+i)
  # Create bootstrap data set by sampling original observations w/ replacement  
  bootdat <- y[sample(1:nobs, nobs, replace=TRUE)] 
  # Calculate bootstrap statistic
  glmboot <- glm(bootdat ~ 1)
  bootcoefs[i] <- coef(glmboot)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 1))
hist(bootcoefs, main = expression(paste("Bootstrap distribution of ", hat(beta)[0])), xlab = "")


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
# Calculate bootstrap standard errors
  boot.se = sd(bootcoefs)

# boostrap-normal CI
  boot.normal = c(
        (preds$fit+boot.se*qnorm(0.05))[1],
        (preds$fit+boot.se*qnorm(0.95))[1]
        )

# bootstrap percentile
confdat.boot.pct <- quantile(bootcoefs, probs = c(0.05, 0.95))



## ----eval=TRUE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
confdata <- data.frame(LCL=c(boot.normal[1],CI.Normal[1],confdat.boot.pct[1]),
           UCL=c(boot.normal[2],CI.Normal[2],confdat.boot.pct[2]),
           method=c("Normal Assumption", "Bootstrap-Normal", "Bootstrap-percentile")
          )
confdata$estimate <- rep(coef(model1.0),3)
library(ggplot2)
ggplot(confdata, aes(y = estimate, x = " ", col = method)) + 
  geom_point() +
  geom_pointrange(aes(ymin = LCL, ymax = UCL, size = 0.1),linewidth = 3,  
                  position = position_dodge(width = 0.9))



## ----echo=FALSE, include=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
n = 100
b0=1
b1=-0.01
b2=0.02
b3=0.0005

set.seed(543543)
x1 = seq(40,100,length.out=n)+rnorm(n,0,10)
x2 = seq(10,500,length.out=n)+rnorm(n,0,2)
sigma=2

mu = b0 + b1*x1 + b2*x1 + b3*(x1*x2)

set.seed(43441)
y = rnorm(n, mu,sigma)
y[which(y<0)]=0

dat = data.frame(y=y,
                 temp = x1,
                 dist.center = x2
                 )


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
  head(dat)


## ----echo=FALSE, include=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
  hist(dat$y)
  hist(dat$temp)
  hist(dat$dist.center)


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
  model = lm(y ~ temp+dist.center, data=dat)
  summary(model)


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
equatiomatic::extract_eq(model)

equatiomatic::extract_eq(model, use_coefs = TRUE)


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
  model = lm(y ~ I(temp - mean(temp)) + dist.center, data=dat)
  summary(model)


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
  dat$temp.sc = scale(dat$temp)
  dat$dist.sc = scale(dat$dist.center)


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
  mean(dat$temp.sc)
  sd(dat$temp.sc)


## ----echo=FALSE, include=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
  par(mfrow=c(1,2))
  hist(dat$temp)
  hist(dat$temp.sc)
  


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
  model = lm(y ~ temp.sc + dist.sc, data=dat)
  summary(model)


## ----echo=FALSE,eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
model = lm(y ~ I(temp - mean(temp)) + dist.center, data=dat)


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
# Plot marginal predictions of dist.center
  marginaleffects::plot_predictions(model, condition=c("dist.center"))


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
# Plot marginal predictions of dist.center
  plot1 = marginaleffects::plot_predictions(model, condition=c("dist.center"))
  plot1+geom_point(data=dat, aes(x=dist.center,y=y))


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
marginaleffects::plot_predictions(model, condition=list("temp","dist.center"))


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
marginaleffects::plot_predictions(model, condition=list("temp","dist.center" = 0:5))


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
newdata = expand.grid(dat$temp,0:5)
colnames(newdata)=c("temp","dist.center")

preds = predict(model,newdata = newdata,interval="confidence", level=0.95)

pred.plot = data.frame(newdata,preds)


## ----echo=FALSE, include=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------

ggplot(data=pred.plot) +
  geom_line(aes(temp, fit, group = dist.center,color = dist.center), linewidth = 1.1)+ 
  geom_ribbon(aes(x = temp, ymax = upr, ymin = lwr, group = dist.center, color = dist.center), alpha = 0.3) +
  theme_bw()
                       


## ----echo=TRUE, include=TRUE, eval=TRUE,fig.align='center'------------------------------------------------------------------------------------------------
hist(model$residuals)


## ----echo=TRUE, include=TRUE, eval=TRUE,fig.align='center'------------------------------------------------------------------------------------------------
plot(model,1)


## ----echo=TRUE, include=TRUE, eval=TRUE,fig.align='center'------------------------------------------------------------------------------------------------
plot(model,3)


## ----echo=TRUE, include=TRUE, eval=TRUE,fig.align='center'------------------------------------------------------------------------------------------------
plot(model,2)


## ----echo=TRUE, include=TRUE, eval=TRUE,fig.align='center'------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(model,4)
plot(model,5)


## ----echo=TRUE, include=TRUE, eval=TRUE-------------------------------------------------------------------------------------------------------------------
library(ggResidpanel)
resid_panel(model)

