## ----echo=FALSE, eval=TRUE-----------------------------------------------------------------------------------------------------------------
N.groups = 5
n.group = 50

mu.overall = log(20)
sd.overall = 3

set.seed(432423)
mu.group = rnorm(N.groups, mu.overall,sd.overall)
#exp(mu.group)

y=c()
for(i in 1:N.groups){
  set.seed(432423+i)
y = c(y,rpois(n.group, exp(mu.group[i])))
}

x = rep(c("Veg1","Veg2","Veg3","Veg4","Veg5"),each=n.group)
dat = data.frame(y=y,x=x, effort = 5)


## ----echo=FALSE, eval=TRUE-----------------------------------------------------------------------------------------------------------------
head(dat)

library(ggplot2)
ggplot(dat, aes(x=x, y=y, fill=x)) + 
    geom_boxplot()+theme(text = element_text(size = 20)) 



## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
library(glmmTMB)
library(broom.mixed)

model1 = glmmTMB(y~1, family=poisson(link="log"),data=dat)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
summary(model1)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
marginaleffects::predictions(model1,type = "response")


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
model2 = glmmTMB(y~x, family=poisson(link="log"),data=dat,
                 contrasts = list(x = "contr.sum"))


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
summary(model2)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
marginaleffects::predictions(model2, 
                             newdata = data.frame(x=c("Veg1","Veg2","Veg3","Veg4","Veg5")),
                             re.form=NA)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
model3 = glmmTMB(y~1+(1|x), family=poisson(link="log"),data=dat)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
summary(model3)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
ranef(model3)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
broom.mixed::tidy(model3, effects = "ran_vals", conf.int = TRUE)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
confint(model3, component = c("all"))


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
fixef(model3)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
#Predictions - does not include RE uncertainty
preds = predict(model3,
                newdata=data.frame(x=c("Veg1","Veg2","Veg3","Veg4","Veg5")),
                type="link",
                re.form=NULL,
                se.fit = TRUE
                )


preds$LCL = exp(preds$fit-1.96*preds$se.fit)
preds$UCL = exp(preds$fit+1.96*preds$se.fit)
preds$fit = exp(preds$fit)

data.frame(preds)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
#A typical site
predict(model3,type="response",re.form=NA)[1]


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
library(lme4)
model3b = glmer(y~1+(1|x), family=poisson(link="log"),data=dat)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
equatiomatic::extract_eq(model3b)


## ----echo=FALSE, eval=TRUE-----------------------------------------------------------------------------------------------------------------
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


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
re.model = glmer(y~cov+(cov|veg), family=poisson(link="log"),data=dat2)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
summary(re.model)


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
marginaleffects::plot_predictions(re.model,
                                  condition=c("cov","veg"),
                                  type="link",
                                  re.form=NULL
                                  )


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
marginaleffects::plot_predictions(re.model,
                                  condition=c("cov","veg"),
                                  type="link",
                                  re.form=NA
                                  )


## ----echo=TRUE, eval=TRUE------------------------------------------------------------------------------------------------------------------
equatiomatic::extract_eq(re.model)

