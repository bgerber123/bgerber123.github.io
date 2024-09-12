## ----eval=TRUE,echo=FALSE-----------------------------------------------------------------------------------------------------------------------
  n=120
  site=as.factor(rep(c("Site 1","Site 2", "Site 3"),n/3))
  set.seed(354)
  herb.cover = rnorm(n, 0, 1)
  dist.trail = rnorm(n, 0, 1)
  beta=c(0,-1,0.5,0.25,1,-0.25)
  X=model.matrix(~site+herb.cover*dist.trail)
  lambda = exp(X%*%beta)
  set.seed(3434)
  y = rpois(n, lambda)
  dat = data.frame(y=y,site=site,herb.cover=herb.cover,dist.trail=dist.trail)
  hist(dat$y,xlab="Pika Counts",breaks=20)  


## ----echo=FALSE, echo=FALSE---------------------------------------------------------------------------------------------------------------------
library(ggplot2)
  ggplot(dat, aes(x=y, fill=site)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080",
                               "yellow")) +
    labs(fill="")


## ----eval=TRUE,echo=TRUE------------------------------------------------------------------------------------------------------------------------
model = glm(y~site,
            data=dat, 
            family = poisson(link = 'log')
            )



## ----eval=TRUE,echo=TRUE------------------------------------------------------------------------------------------------------------------------
summary(model)


## ----eval=TRUE,echo=TRUE------------------------------------------------------------------------------------------------------------------------
model$converged
model$method
model$boundary
model$iter
model$control


## ----echo=TRUE, include=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------
  #Here is our negative log-likelihood function with three
  #parameters - beta0, beta1, and beta2 (1)
  #inputs = design matrix X
  neg.log.like = function(par,X) {
    
    #linear model of parameters par and design matrix (X)
    lam=par[1]*X[,1]+par[2]*X[,2]+par[3]*X[,3]
    
    #neg log-likelihood
    sum(-dpois(y,lambda = exp(lam),log = TRUE))
  }


## ----echo=TRUE, include=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------
#Use optim function with initial values and define the lower and upper limits of the possible values
  fit1 <- optim(
    par = c(0,0,0), #start
    X = X,
    fn = neg.log.like,
    method = "L-BFGS-B",
    lower = c(-10, -10, -10),
    upper = c(10, 10, 10)
  )



## ----echo=FALSE, include=TRUE, eval=TRUE--------------------------------------------------------------------------------------------------------
a =rbind(fit1$par,
      coef(model)
      )
rownames(a)=c("glm.fit","ours")
a


## ----eval=TRUE,echo=TRUE------------------------------------------------------------------------------------------------------------------------
dat$site.re = relevel(dat$site,ref="Site 2")
levels(dat$site.re) 


## ----eval=TRUE,echo=TRUE------------------------------------------------------------------------------------------------------------------------
model2 = glm(y~site.re,
             data = dat, 
             family = poisson(link = 'log')
            )
summary(model2)


## ----eval=TRUE,echo=FALSE-----------------------------------------------------------------------------------------------------------------------
a = rbind(coef(model),
          coef(model2)
          )
rownames(a)=c("Model1","Model2")

round(a,digits=4)



## ----eval=TRUE,echo=FALSE-----------------------------------------------------------------------------------------------------------------------
b = rbind(predict(model,type="response")[1:5],
      predict(model2,type="response")[1:5]
      )
rownames(b)=c("Model1","Model2")
round(b,digits=4)


## -----------------------------------------------------------------------------------------------------------------------------------------------
model.matrix(~dat$site)[1:5,]
model.matrix(~dat$site.re)[1:5,]


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------
  model3=glm(y~site, 
             data = dat, 
             family = poisson(link = 'log'),
             contrasts = list(site = contr.sum)
            )


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------
coef(model3)


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------
sum(coef(model3)[-1]*(-1))


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------
mean.group = aggregate(y, by=list(site=site),FUN=mean)
mean.group


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------
#Site 1
exp(0.3584266 + 0.2964994)
as.numeric(predict(model3,type="response")[1])


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------
#Site 2
exp(0.3584266 + -0.7151015 )
as.numeric(predict(model3,type="response")[2])


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------
#Site 3
exp(0.3584266 + 0.4186021)
as.numeric(predict(model3,type="response")[3])


## ----echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------------------------------------------
  model4=glm(y~0+site, 
             data=dat, 
             family = poisson(link = 'log')
            )

# No shared intercept
head(model.matrix(~0 + site, data = dat), n = 6)



## ----eval=TRUE,echo=FALSE-----------------------------------------------------------------------------------------------------------------------
c =rbind(coef(model),
         coef(model2),
         coef(model3),
         coef(model4)
        )
rownames(c)=c("Model1","Model2","Model3","Model4")
c


## ----eval=TRUE,echo=FALSE-----------------------------------------------------------------------------------------------------------------------
d=rbind(predict(model,type="response")[1:5],
      predict(model2,type="response")[1:5],
      predict(model3,type="response")[1:5],
      predict(model4,type="response")[1:5]
      )
rownames(d)=c("Model1","Model2","Model3","Model4")
d



## ----eval=TRUE,echo=FALSE-----------------------------------------------------------------------------------------------------------------------
d=rbind(model$deviance,
      model2$deviance,
      model3$deviance,
      model4$deviance
      )
rownames(d)=c("Model1","Model2","Model3","Model4")
d



## ----eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------------------------------
## 
## X = model.matrix(~Independent Variables)
## 
## #all variables in dat (additive)
## X = model.matrix(~., data = dat)
## 
## #all variables in dat (pair-wise interactions)
## X = model.matrix(~.^2, data = dat)
## 
## #all variables in dat (three way- interactions)
## X = model.matrix(~.^3, data = dat)
## 
## model = glm(y~ 0 + X, ...)
## 


## ----echo=FALSE,eval=TRUE-----------------------------------------------------------------------------------------------------------------------
set.seed(54345)
dat$area=rpois(n,dat$y*2)
dat$area[which(dat$area==0)]=2


## ----echo=TRUE,eval=TRUE------------------------------------------------------------------------------------------------------------------------

model.rate1 = glm(y~site+offset(log(area)),
            data=dat, 
            family = poisson(link = 'log')
            )

model.rate2 = glm(y~site,
            offset = log(area),
            data=dat, 
            family = poisson(link = 'log')
            )

coef(model.rate1)
coef(model.rate2)



## ----echo=TRUE,eval=TRUE------------------------------------------------------------------------------------------------------------------------
head(dat)


## ----echo=TRUE,eval=TRUE------------------------------------------------------------------------------------------------------------------------
m1 = glm(y~site+herb.cover,data=dat,family = poisson(link = 'log'))
marginaleffects::plot_predictions(m1, condition=c("herb.cover","site"),type="link")


## ----echo=TRUE,eval=TRUE------------------------------------------------------------------------------------------------------------------------
marginaleffects::plot_predictions(m1, 
                                  condition=c("herb.cover","site"),
                                  type="response")


## ----echo=TRUE,eval=TRUE------------------------------------------------------------------------------------------------------------------------
m2 = glm(y~dist.trail+herb.cover,data=dat,family = poisson(link = 'log'))
marginaleffects::plot_predictions(m2, 
                                  condition=c("dist.trail","herb.cover"),
                                  type="link")


## ----echo=TRUE,eval=TRUE, fig.width=5, fig.height=5---------------------------------------------------------------------------------------------
marginaleffects::plot_predictions(m2, 
                                  condition=c("dist.trail","herb.cover"),
                                  type="response")


## ----echo=TRUE,eval=TRUE, fig.width=10, fig.height=5--------------------------------------------------------------------------------------------
m3 = glm(y~site*herb.cover,data=dat,family = poisson(link = 'log'))
m3 = glm(y~site+herb.cover+site:herb.cover,data=dat,family = poisson(link = 'log'))
head(model.matrix(~site*herb.cover,data=dat))


## ----echo=TRUE,eval=TRUE------------------------------------------------------------------------------------------------------------------------
summary(m3)


## ----echo=TRUE,eval=TRUE, fig.width=10, fig.height=5--------------------------------------------------------------------------------------------
marginaleffects::plot_predictions(m3,condition=c("herb.cover","site"))


## ----echo=TRUE,eval=TRUE------------------------------------------------------------------------------------------------------------------------
m4 = glm(y~herb.cover*dist.trail,data=dat, family = poisson(link = 'log'))
m4 = glm(y~herb.cover+dist.trail+herb.cover:dist.trail,data=dat,family = poisson(link = 'log'))
marginaleffects::plot_predictions(m4, condition=c("herb.cover","dist.trail"))


## ----echo=TRUE,eval=TRUE, fig.width=10, fig.height=5--------------------------------------------------------------------------------------------
m5 = glm(y~poly(dist.trail,2),data=dat,family = poisson(link = 'log'))
m5 = glm(y~dist.trail+I(dist.trail^2),data=dat,family = poisson(link = 'log'))
marginaleffects::plot_predictions(m5,condition=c("dist.trail"),type="link")

## ----echo=TRUE,eval=TRUE, fig.width=10, fig.height=5--------------------------------------------------------------------------------------------
m5 = glm(y~poly(dist.trail,2),data=dat,family = poisson(link = 'log'))
m5 = glm(y~dist.trail+I(dist.trail^2),data=dat,family = poisson(link = 'log'))
marginaleffects::plot_predictions(m5,condition=c("dist.trail"),type="response")

