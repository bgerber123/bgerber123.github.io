library(ggplot2)
library(sjPlot)

#Goals: glm function, linear regression, categorical and 
#continuours variables, additive models, interactions. 
#prediction, confidence intervals. bootstrapping.
#linear regression assumptions

#Intercept only model

#Simulate data
n=100
mu=10
sigma=2
set.seed(43243)
y=rnorm(n,mean=mu, sd=sigma)

#Fit model with intercept only
#These functions will do the same thing
  model1 = glm(y~1, family=gaussian(link = identity))
  model1.1 = glm(y~1)
#Same results  
  model1$coefficients
  model1.1$coefficients

#Summary of model results  
  summary(model1)

#Predict response for all data
  preds=predict(model1, se.fit = TRUE)
  preds
# Get 90% confidence intervals (Type I error = 0.1)
  (preds$fit+preds$se.fit*qnorm(0.05))[1]
  (preds$fit+preds$se.fit*qnorm(0.95))[1]

  CI.Normal=confint(model1, level=0.9)

# Bootstrapping  
# For Details: https://fw8051statistics4ecologists.netlify.app/boot.html  
  
#Instead of relying on the 95% intervals from an assumed
#normal distribution, we will create a distribution by 
#resampling our data.

nboot <- 1000 # number of bootstrap samples
nobs <- length(y)
bootcoefs <- rep(NA, nboot)
for(i in 1:nboot){
  set.seed(43243+i)
  # Create bootstrap data set by sampling original observations w/ replacement  
  bootdat <- y[sample(1:nobs, nobs, replace=TRUE)] 
  # Calculate bootstrap statistic
  lmboot <- glm(bootdat ~ 1)
  bootcoefs[i] <- coef(lmboot)
}
par(mfrow = c(1, 1))
hist(bootcoefs, main = expression(paste("Bootstrap distribution of ", hat(beta)[0])), xlab = "")

# Calculate bootstrap standard errors
  boot.se=sd(bootcoefs)

#boostrap-normal CI
boot.normal=c(
        (preds$fit+boot.se*qnorm(0.05))[1],
        (preds$fit+boot.se*qnorm(0.95))[1]
)


# bootstrap percentile
confdat.boot.pct <- quantile(bootcoefs, probs = c(0.05, 0.95))

confdata <- data.frame(LCL=c(boot.normal[1],CI.Normal[1],confdat.boot.pct[1]),
           UCL=c(boot.normal[2],CI.Normal[2],confdat.boot.pct[2]),
           method=c("z-based", "bootstrap-Normal", "bootstrap-percentile")
          )
confdata$estimate <- rep(coef(model1),3)

ggplot(confdata, aes(y = estimate, x = " ", col = method)) + 
  geom_point() +
  geom_pointrange(aes(ymin = LCL, ymax = UCL),  
                  position = position_dodge(width = 0.9))


##############################
#Categorical variable
n

x=as.factor(rep(c("Site 1","Site 2"),n/2))
levels(x)
x.var=model.matrix(~x)[,2]
b0=50
b1=-20

mu=b0+b1*x.var

set.seed(43243)
y=rnorm(n,mean=mu,sd=4)

#fit the model- see how we did

model2=glm(y~x)
model2.1=glm(y~x.var)

coef(model2)
coef(model2.1)

summary(model2)

#change intercept meaning

x.relev=relevel(x,ref="Site 2")
levels(x.relev)

#fit the model again
  model2.3=glm(y~x.relev)

#Look at coefs  
  rbind(coef(model2),coef(model2.3))

#compare predictions  
predict(model2)[1:2]  
predict(model2.3)[1:2]  

##############################
#Four levels of categorical variable


x=as.factor(rep(c("Site 1","Site 2","Site 3", "Site 4"),n/4))
levels(x)
x.var=model.matrix(~x)[,2:4]
b0=50
b1=-20
b2=-200
b3=100
mu=b0+b1*x.var[,1]+b2*x.var[,2]+b3*x.var[,3]


#True mean group-level values
  unique(mu)
#Grand Mean
  mean(unique(mu))

set.seed(43243)
y=rnorm(n,mean=mu,sd=4)

#fit the model- see how we did
  model3=glm(y~x)
  model3.1=glm(y~x.var)
  
  coef(model3)
  coef(model3.1)

#Use effect coding to make the intercept the grand mean
  model3.2=glm(y~x,contrasts = list(x = contr.sum))

  coef(model3.2)

#The coefficient for site-level 4 (difference from the grand mean)
  coef(model3.2)[1]+sum(coef(model3.2)[-1])

#Predict the values and compare them to the true means for
#each site
  rbind(unique(mu),
  predict(model3.2)[1:4])

  
############################
#A continuous and categorical variable 
#Additive model
  
x=as.factor(rep(c("Site 1","Site 2"),n/2))
levels(x)
x.var=model.matrix(~x)[,2]

x2=rpois(n,100)

b0=50
b1=-50
b2=4

mu=b0+b1*x.var+b2*x2

set.seed(43243)
y=rnorm(n,mean=mu,sd=50)

#fit the model- see how we did

model4=glm(y~x+x2)

coef(model4)

summary(model4)

newdata=expand.grid(x,x2)
head(newdata)
colnames(newdata)=c("x","x2")

#Confidence intervals of coefs
confint(model4)

preds=predict(model4,newdata=newdata,type="response",
              se.fit = TRUE)


plot_model(model4, type = "pred", terms = c("x"))

plot_model(model4, type = "pred", terms = c("x2","x"))


############################
#A continuous and categorical variable (interaction)

x=as.factor(rep(c("Site 1","Site 2"),n/2))
levels(x)
x.var=model.matrix(~x)[,2]

x2=rpois(n,100)

#x2.scaled=scale(x2,center = TRUE,scale=TRUE)

b0=50
b1=-50
b2=4
b3=-20

mu=b0+b1*x.var+b2*x2+(x.var*x2)*b3

set.seed(43243)
y=rnorm(n,mean=mu,sd=10)

#fit the model- see how we did

model5=glm(y~x*x2)

model5.1=glm(y~x+x2+x:x2)

rbind(coef(model5),coef(model5.1))

summary(model5)

#Confidence intervals of coefs
confint(model5)



plot_model(model5, type = "pred", terms = c("x"))

plot_model(model5, type = "pred", terms = c("x2","x"))




