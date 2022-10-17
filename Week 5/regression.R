## ---- echo=FALSE,eval=TRUE------------------------------
n=1000
b0=10
b1=2
x=rnorm(n,40,10)
mu=b0+b1*x
y=rnorm(n,mu,sd=30)

plot(x,y)


## ----echo=FALSE,eval=TRUE, fig.align='center',width="1000"----
plot(x,y,cex.lab=1.3,cex.axis=1.3, main="Intercept (beta0) = 9.06 \n Slope of x (beta1) = 2.0")
lm.out=lm(y~x)
abline(lm.out,lwd=3,col=3)

legend("topleft",col=c("green"),lty=2,legend=c("Mean"),lwd=4)



## ----echo=FALSE,eval=TRUE, fig.align='center',width="1000"----
plot(x,y,cex.lab=1.3,cex.axis=1.3, main="Intercept (beta0) = 9.06 \n Slope of x (beta1) = 2.0")
lm.out=lm(y~x)
abline(lm.out,lwd=3,col=3)

newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence", level = 0.95)

matlines(newx, conf_interval[,2:3], col = "blue", lty=2,lwd=4)

legend("topleft",col=c("green","blue"),lty=2,legend=c("Mean","Confidence Interaval (mean)"),lwd=4)



## ----echo=FALSE,eval=TRUE, fig.align='center',width="1000"----
plot(x,y,cex.lab=1.3,cex.axis=1.3, main="Intercept (beta0) = 9.06 \n Slope of x (beta1) = 2.0")
lm.out=lm(y~x)
abline(lm.out,lwd=3,col=3)

newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence", level = 0.95)

pred_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="prediction", level = 0.95)


matlines(newx, conf_interval[,2:3], col = "blue", lty=2,lwd=4)

matlines(newx, pred_interval[,2:3], col = "purple", lty=2,lwd=4)

legend("topleft",col=c("green","blue","purple"),lty=2,legend=c("Mean","Confidence Interaval (mean)", "Prediction Interval (data)"),lwd=4)



## ----eval=TRUE,echo=TRUE,fig.align='center'-------------
#Simulate Data for this model

#Setup parameters
  n=100 # sample size
  mu=10 # true mean
  sigma=2 # true std.dev

# Simulate a data set of observations
  set.seed(43243)
  y=rnorm(n,mean=mu, sd=sigma)


## ----eval=TRUE,echo=TRUE,fig.align='center'-------------
  hist(y)


## ----eval=TRUE,echo=TRUE--------------------------------
# Fit model/hypothesis using maximum likelihood
  model1 = glm(y~1, family=gaussian(link = identity))

  model1.1 = glm(y~1)

# Compare Results  
  rbind(model1$coefficients,model1.1$coefficients)


## ----eval=TRUE,echo=TRUE--------------------------------
    
# Summary of model results  
  summary(model1)


## ----eval=TRUE,echo=TRUE--------------------------------
#Predict response for all data
  preds=predict(model1, se.fit = TRUE)
  preds


## ----eval=TRUE,echo=TRUE--------------------------------
# Get 90% confidence intervals (Type I error = 0.1)
  (preds$fit+preds$se.fit*qnorm(0.05))[1]
  (preds$fit+preds$se.fit*qnorm(0.95))[1]


  CI.Normal=confint(model1, level=0.9)
  CI.Normal
  


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------

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


## -------------------------------------------------------
par(mfrow = c(1, 1))
hist(bootcoefs, main = expression(paste("Bootstrap distribution of ", hat(beta)[0])), xlab = "")


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
# Calculate bootstrap standard errors
  boot.se=sd(bootcoefs)

# boostrap-normal CI
  boot.normal=c(
        (preds$fit+boot.se*qnorm(0.05))[1],
        (preds$fit+boot.se*qnorm(0.95))[1])

# bootstrap percentile
confdat.boot.pct <- quantile(bootcoefs, probs = c(0.05, 0.95))



## ---- eval=TRUE, echo=FALSE-----------------------------
confdata <- data.frame(LCL=c(boot.normal[1],CI.Normal[1],confdat.boot.pct[1]),
           UCL=c(boot.normal[2],CI.Normal[2],confdat.boot.pct[2]),
           method=c("Normal Assumption", "Bootstrap-Normal", "Bootstrap-percentile")
          )
confdata$estimate <- rep(coef(model1),3)
library(ggplot2)
ggplot(confdata, aes(y = estimate, x = " ", col = method)) + 
  geom_point() +
  geom_pointrange(aes(ymin = LCL, ymax = UCL),  
                  position = position_dodge(width = 0.9))



## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
# Setup data
  x=as.factor(rep(c("Site 1","Site 2"),n/2))
  levels(x)
  
# Turn the factor into 0 and 1's
  head(model.matrix(~x))
  
  x.var=model.matrix(~x)[,2]


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
# Parameters  
  b0=50
  b1=-20
  mu=b0+b1*x.var

# Sample Data
  set.seed(43243)
  y=rnorm(n,mean=mu,sd=4)



## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#fit the model 
  model2=glm(y~x)
  model2.1=glm(y~x.var)

#comparison  
  rbind(coef(model2), coef(model2.1))


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
summary(model2)


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#change intercept meaning
  x.relev=relevel(x,ref="Site 2")
  levels(x.relev)


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#fit the model again
  model2.2=glm(y~x.relev)

#Look at coefs  
  rbind(coef(model2),coef(model2.2))


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#compare predictions  
  predict(model2)[1:2]  
  predict(model2.2)[1:2]  


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#Setup Data
  x=as.factor(rep(c("Site 1","Site 2","Site 3", "Site 4"),n/4))
  levels(x)

#Convert factors to 0 and 1's
  head(model.matrix(~x))

  x.var=model.matrix(~x)[,2:4]


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
# Set Parameters  
  b0=50 #Site 1
  b1=-20 #Diff of site 2 to site 1
  b2=-200 #Diff of site 3 to site 1
  b3=100 #Diff of site 4 to site 1
  
# Mean  
  mu=b0+b1*x.var[,1]+b2*x.var[,2]+b3*x.var[,3]

#True mean group-level values
  unique(mu)
  
#Grand Mean
  mean(unique(mu))


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
# Simulate Data  
  set.seed(43243)
  y=rnorm(n,mean=mu,sd=4)


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#fit the model- see how we did
  model3=glm(y~x)
  model3.1=glm(y~x.var)

#Compare coefs    
  rbind(coef(model3),
  coef(model3.1))


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------

#Use effect coding to make the intercept the grand mean
  model3.2=glm(y~x,contrasts = list(x = contr.sum))


# Intercept = grand mean of group-means
# Coef 1 = effect difference of Site 1 from Grand Mean
# Coef 2 = effect difference of Site 2 from Grand Mean
# Coef 3 = effect difference of Site 3 from Grand Mean

  coef(model3.2)



## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#The coefficient for site-level 4 (difference from the grand mean)
  coef(model3.2)[1]+sum(coef(model3.2)[-1])

#Predict the values and compare them to the true means for
#each site
  rbind(unique(mu),
  predict(model3.2)[1:4])


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#A continuous and categorical variable 
  x=as.factor(rep(c("Site 1","Site 2"),n/2))
  levels(x)
  x.var=model.matrix(~x)[,2]


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#Simulate x2 variable
  set.seed(54334)
  x2=rpois(n,100)

#Parameters
  b0=50
  b1=-50
  b2=4

#Mean  
  mu=b0+b1*x.var+b2*x2

#Simualte Date  
  set.seed(43243)
  y=rnorm(n,mean=mu,sd=50)

# fit the model
  model4=glm(y~x+x2)

  coef(model4)

#Confidence intervals of coefs
  confint(model4)



## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
# Summary  
  summary(model4)


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------

# Fitted Values
  newdata=expand.grid(x,x2)
  head(newdata)
  colnames(newdata)=c("x","x2")


  preds=predict(model4,newdata=newdata,type="response",
              se.fit = TRUE)


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
library(sjPlot)
plot_model(model4, type = "pred", terms = c("x"))


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
plot_model(model4, type = "pred", terms = c("x2","x"))


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------

# Simulate Variables
  x=as.factor(rep(c("Site 1","Site 2"),n/2))
  levels(x)
  x.var=model.matrix(~x)[,2]

  set.seed(5453)
  x2=rpois(n,100)

# Parameters 
  b0=50
  b1=-50
  b2=4
  b3=-20

# Mean  
  mu = b0+b1*x.var+b2*x2+b3*(x.var*x2)

#Simulate Data
  set.seed(43243)
  y=rnorm(n,mean=mu,sd=10)

# fit the model
  model5=glm(y~x*x2)
  model5.1=glm(y~x+x2+x:x2)

#comparison  
  rbind(coef(model5),coef(model5.1))


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#Confidence intervals of coefs
  confint(model5)


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
#Summary  
  summary(model5)


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
plot_model(model5, type = "pred", terms = c("x"))


## ---- echo=TRUE, include=TRUE, eval=TRUE----------------
plot_model(model5, type = "pred", terms = c("x2","x"))

