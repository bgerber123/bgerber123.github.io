## ----knitr, echo=FALSE,results='hide'---------------------------------------------------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(magrittr)
library(knitr)

#knitr::purl(input="../FW552/Ratio.qmd",output="../FW552/Ratio.r")


## ---------------------------------------------------------------------------------------------------------------------------------------
tk =data.frame(Indiv=1:6,
               height = c(32,24,28,20,36,25), #inches
               weight = c(25,20,26,13,33,26) #pounds
               )

mu.weight = mean(tk$weight)
mu.height = mean(tk$height)

tk %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)



## ----fig.width=6------------------------------------------------------------------------------------------------------------------------
par(cex.lab=1.5,cex.main=1.5,cex=1.5)
plot(tk$height,tk$weight,xlab="Height (in)",ylab="Weight (lbs)",pch=18,cex=2)


## ---------------------------------------------------------------------------------------------------------------------------------------
indiv.index=t(utils::combn(1:6,4))

weight.all=data.frame(cbind(
            matrix(1:15,ncol=1),
            matrix(tk$weight[indiv.index],ncol=4,byrow = TRUE)))

weight.all$means = apply(weight.all[,-1],1,mean)

colnames(weight.all)=c("Sample.Number","Indiv.1","Indiv.2","Indiv.3","Indiv.4","means")

weight.all %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ---------------------------------------------------------------------------------------------------------------------------------------
indiv.index=t(utils::combn(1:6,4))

height.all=data.frame(cbind(
            matrix(1:15,ncol=1),
            matrix(tk$height[indiv.index],ncol=4,byrow = TRUE)))

height.all$means = apply(height.all[,-1],1,mean)

colnames(height.all)=c("Sample.Number","Indiv.1","Indiv.2","Indiv.3","Indiv.4","means")

height.all %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)


## ---------------------------------------------------------------------------------------------------------------------------------------

ratio.df = data.frame(cbind(
            matrix(1:15,ncol=1),
            height.all$means,
            weight.all$means),
            weight.all$means/height.all$means*mu.height)
colnames(ratio.df)=c("Sample.Number","Primary.Weight","Aux.Height", "Ratio")

ratio.mean = mean(ratio.df$Ratio)

ratio.df %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)



## ----echo=FALSE-------------------------------------------------------------------------------------------------------------------------
#calculate expected variance for the weight
exp.sample.var = mean(apply(weight.all[,c(2,3,4,5)],1,var)/4 * (1-4/6))

#calculate expected variance of ratio estimator
y=as.matrix(weight.all[,c(2,3,4,5)])
x=as.matrix(height.all[,c(2,3,4,5)])
ratio.var=rep(0,nrow(y))
for(i in 1:nrow(y)){
ratio.var[i] = 1/(4-1)*sum((y[i,]- (mean(y[i,])/mean(x[i,]))*x[i,])^2)
}
var.pop.mean = ((6-4)/6)*(ratio.var/4)

exp.ratio.var= mean(var.pop.mean)


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------
# Setup
  N = 500
  set.seed(453543)
  x = rpois(N, 500)

# Fixed 
  beta0 = -5
  beta1 = 4
  sigma = 100

# Random variation- inducing some not perfect correlation
  set.seed(1453543)
  epsilon = rnorm(N, 0, sigma)

# Derive y  
  y = beta0 + beta1*x + epsilon
  

# True population mean
  mean(y)
  


## ---------------------------------------------------------------------------------------------------------------------------------------
plot(x,y)


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------
nsim = 10000
pred.model.coef = beta1.save = save.mean=rep(NA, nsim)


for(i in 1:nsim){
  
# Sample size
  n = 100  
  
  #simple random sample
  index = sample(1:N,n)
  
  #Eqns in Section 8.1
  beta1.save[i] = sum((x[index]-mean(x[index]))*(y[index]-mean(y[index])))/sum((x[index]-mean(x[index]))^2)
  beta0 = mean(y[index]) - beta1.save[i] * mean(x[index])
  save.mean[i] =  beta0 + beta1.save[i]*mean(x)
  
  pred.model.coef[i] = coef(lm(y[index]~x[index]))[2]
  
}




## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------
hist(beta1.save,breaks=25)
abline(v=beta1,lwd=3,col=2)
mean(beta1.save)-beta1


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------
hist(pred.model.coef,breaks=25)
abline(v=beta1,lwd=3,col=2)
mean(beta1.save)-beta1


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------
hist(save.mean)
abline(v=mean(y),lwd=3,col=2)
mean(save.mean)-mean(y)


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------
nsim=10000
pred.model.coef= beta1.save = save.mean=rep(NA, nsim)

 N = 500
  set.seed(453543)
  x = rpois(N, 500)

# Fixed 
  beta0 = -5
  beta1 = 4
  sigma = 100


  

for(i in 1:nsim){

# True is now changing!    
  epsilon = rnorm(N, 0, sigma)
  y = beta0 + beta1*x + epsilon

# Sample size
  n = 100  
  
# simple random sample
  index=sample(1:N,n)
  
# Eqns in Section 8.1
  beta1.save[i] = sum((x[index]-mean(x[index]))*(y[index]-mean(y[index])))/sum((x[index]-mean(x[index]))^2)
  beta0 = mean(y[index]) - beta1.save[i] * mean(x[index])
  save.mean[i] =  beta0 + beta1.save[i]*mean(x)
  
  pred.model.coef[i]=coef(lm(y[index]~x[index]))[2]
  
}



## ---------------------------------------------------------------------------------------------------------------------------------------
hist(beta1.save,breaks=25)
abline(v=beta1,lwd=3,col=2)
mean(beta1.save)-beta1

