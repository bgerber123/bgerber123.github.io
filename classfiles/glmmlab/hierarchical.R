## ----echo=FALSE, eval=TRUE----------------------------------------------------------------------------------------------------------------------------------
N.groups = 5
n.group = c(2,20,20,10,10)

mu.overall = log(20)
sd.overall = 1

set.seed(432423)
mu.group = rnorm(N.groups, mu.overall,sd.overall)
epsilon=0.1
#exp(mu.group)

y=c()
for(i in 1:N.groups){
  set.seed(432423+i)
y = c(y,rpois(n.group[i], exp(mu.group[i]+rnorm(1,0,epsilon))))
}

x = c(rep("Veg1",each=n.group[1]),rep(c("Veg2","Veg3"),each=n.group[2]),rep("Veg4",n.group[4]),rep("Veg5",n.group[5]))
dat = data.frame(y=y,veg=x, effort = 5)



## ----echo=FALSE, eval=TRUE----------------------------------------------------------------------------------------------------------------------------------
head(dat)
table(dat$veg)

library(ggplot2)
ggplot(dat, aes(x=veg, y=y, fill=veg)) + 
    geom_boxplot()+theme(text = element_text(size = 20)) 



## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
library(glmmTMB)
library(broom.mixed)

model1 = glmmTMB(y~1, family=poisson(link="log"),data=dat)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
summary(model1)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
marginaleffects::predictions(model1,type = "response")


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
model2 = glmmTMB(y~veg, family=poisson(link="log"),data=dat,
                 contrasts = list(veg = "contr.sum"))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
summary(model2)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
marginaleffects::predictions(model2, 
                             newdata = data.frame(veg=c("Veg1","Veg2","Veg3","Veg4","Veg5")),
                             re.form=NA)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
model3 = glmmTMB(y~1+(1|veg), family=poisson(link="log"),data=dat)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
summary(model3)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
ranef(model3)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
broom.mixed::tidy(model3, effects = "ran_vals", conf.int = TRUE)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
confint(model3, component = c("all"))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
fixef(model3)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
#Predictions - does not include RE uncertainty
preds = predict(model3,
                newdata=data.frame(veg=c("Veg1","Veg2","Veg3","Veg4","Veg5")),
                type="link",
                re.form=NULL,
                se.fit = TRUE
                )


preds$LCL = exp(preds$fit-1.96*preds$se.fit)
preds$UCL = exp(preds$fit+1.96*preds$se.fit)
preds$fit = exp(preds$fit)

data.frame(preds)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
#A typical site
predict(model3,type="response",re.form=NA)[1]


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
library(lme4)
model3b = glmer(y~1+(1|veg), family=poisson(link="log"),data=dat)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
equatiomatic::extract_eq(model3b)


## ----echo=FALSE,eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
library(plotrix)


m1.est=data.frame(confint(model1))


plot(1:5,rep(-50,5),ylim=c(2,4.2),xaxt='n',xlab="Vegetation",ylab="Coeficient",main="Estimates")
axis(1,at=1:5,lab=c("Veg1","Veg2","Veg3","Veg4","Veg5"))
abline(h=m1.est$Estimate,lwd=3,col=1,lty=1)
abline(h=m1.est$X2.5..,lwd=2,col=1,lty=4)
abline(h=m1.est$X97.5..,lwd=2,col=1,lty=4)
legend("topright",lwd=3,col=1,legend=c("Model 1, (y~1), Complete Pooling"))




## ----echo=FALSE,eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
m2.est=marginaleffects::predictions(model2, 
                             newdata = data.frame(veg=c("Veg1","Veg2","Veg3","Veg4","Veg5")),
                             re.form=NA,type="link")



plot(1:5,rep(-50,5),ylim=c(2,4.2),xaxt='n',xlab="Vegetation",ylab="Coeficient",main="Estimates")
axis(1,at=1:5,lab=c("Veg1","Veg2","Veg3","Veg4","Veg5"))
abline(h=m1.est$Estimate,lwd=3,col=1,lty=1)
abline(h=m1.est$X2.5..,lwd=2,col=1,lty=4)
abline(h=m1.est$X97.5..,lwd=2,col=1,lty=4)
legend("topright",lwd=3,col=1,legend=c("Model 1, (y~1), Complete Pooling"))


plotCI(1:5,y=m2.est$estimate,ui=m2.est$conf.high,li=m2.est$conf.low,pch=18,xaxt='n',add=TRUE,col="purple")



legend("topright",lwd=3,col=c(1,"purple"),legend=c("Model 1, (y~1), Complete Pooling",
                                       "Model 2, (y~x), No Pooling"))



## ----echo=FALSE,eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------


m3.est = predict(model3,
                newdata=data.frame(veg=c("Veg1","Veg2","Veg3","Veg4","Veg5")),
                type="link",
                re.form=NULL,
                se.fit = TRUE
                )

m3.est$LCL = m3.est$fit-1.96*m3.est$se.fit
m3.est$UCL = m3.est$fit+1.96*m3.est$se.fit



plot(1:5,rep(-50,5),ylim=c(2,4.2),xaxt='n',xlab="Vegetation",ylab="Coeficient",main="Estimates")
axis(1,at=1:5,lab=c("Veg1","Veg2","Veg3","Veg4","Veg5"))
abline(h=m1.est$Estimate,lwd=3,col=1,lty=1)
abline(h=m1.est$X2.5..,lwd=2,col=1,lty=4)
abline(h=m1.est$X97.5..,lwd=2,col=1,lty=4)
legend("topright",lwd=3,col=1,legend=c("Model 1, (y~1), Complete Pooling"))

plotCI(1:5,y=m2.est$estimate,ui=m2.est$conf.high,li=m2.est$conf.low,pch=18,xaxt='n',add=TRUE,col="purple")

plotCI(1:5,y=m3.est$fit,ui=m3.est$UCL,li=m3.est$LCL,pch=18,xaxt='n',add=TRUE,col="green")


legend("topright",lwd=3,col=c(1,"purple"),legend=c("Model 1, (y~1), Complete Pooling",
                                       "Model 2, (y~veg), No Pooling",
                                       "Model 3, y~1+(1|veg), Partial Pooling"))



## ----echo=FALSE, eval=TRUE----------------------------------------------------------------------------------------------------------------------------------
N.groups = 10
n.group = 50

mu.beta0 = 0
sd.beta0 = 1

mu.beta1 = 2
sd.beta1 = 0.2


set.seed(14324231)
beta0 = rnorm(N.groups, mu.beta0,sd.beta0)
beta1 = rnorm(N.groups, mu.beta1,sd.beta1)



y=NULL
x=NULL
for(i in 1:N.groups){
  set.seed(432423+i)
  x=c(x,rnorm(n.group))
y = c(y,
      rpois(n.group,exp(beta0[i]+beta1[i]*x))
      )
}

veg = rep(c("Veg1","Veg2","Veg3","Veg4","Veg5"),each=n.group)
dat2 = data.frame(y=y,cov=x,veg=veg)
head(dat2)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
re.model = glmer(y~cov+(cov|veg), family=poisson(link="log"),data=dat2)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
summary(re.model)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
marginaleffects::plot_predictions(re.model,
                                  condition=c("cov","veg"),
                                  type="link",
                                  re.form=NULL
                                  )


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
marginaleffects::plot_predictions(re.model,
                                  condition=c("cov","veg"),
                                  type="link",
                                  re.form=NA
                                  )


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------------------
equatiomatic::extract_eq(re.model)

