## ----echo=FALSE,warning=FALSE,results='hide'----------------------------------
library(tidyverse)
library(kableExtra)
library(magrittr)
library(knitr)


## ----echo=FALSE---------------------------------------------------------------
ponds = data.frame(matrix(nrow=6,ncol=2))
colnames(ponds)=c("Pond","egg.mass")

ponds$Pond=LETTERS[1:6]
ponds$egg.mass = c(2,6,8,10,10,12)

ponds %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)



## ----echo=FALSE---------------------------------------------------------------
mu = mean(ponds$egg.mass)


## ----echo=TRUE----------------------------------------------------------------
choose(6,2)


## ----echo=TRUE----------------------------------------------------------------
utils::combn(LETTERS[1:6],2)


## ----echo=FALSE---------------------------------------------------------------
ponds.letters=t(utils::combn(LETTERS[1:6],2))
ponds.numbers=utils::combn(1:6,2)

ponds.all=data.frame(cbind(matrix(1:15,ncol=1),ponds.letters,matrix(ponds$egg.mass[ponds.numbers],ncol=2,byrow = TRUE)))
colnames(ponds.all)=c("Sample.Number","First Pond","Second Pond","First Value","Second Value")

ponds.all %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 20)



## ----echo=TRUE----------------------------------------------------------------
sample(LETTERS[1:6],2)


## ----echo=TRUE----------------------------------------------------------------
sample(LETTERS[1:6],2)


## ----echo=TRUE----------------------------------------------------------------
sample(LETTERS[1:6],2)


## ----echo=FALSE---------------------------------------------------------------
#library(kableExtra)
#library(tidyr)
ponds.all$Sample.Mean = apply(matrix(ponds$egg.mass[ponds.numbers],ncol=2,byrow = TRUE),1,mean)

ponds.all$Sample.Error = abs(ponds.all$Sample.Mean-8)

#knitr::kable(ponds.all)


ponds.all %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 20)



## ----echo=FALSE---------------------------------------------------------------
ponds.simple = data.frame(Sample.Number = ponds.all$Sample.Number,
                          Sample.Mean = ponds.all$Sample.Mean,
                          Sample.Error = ponds.all$Sample.Mean-8
                          )
ponds.simple %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 20)



## ----echo=FALSE,results='hide'------------------------------------------------
#result above
mean(ponds.simple$Sample.Mean)


## ----echo=FALSE, results='hide'-----------------------------------------------
#result above
sum(ponds.simple$Deviance.Truth)


## ----echo=FALSE---------------------------------------------------------------
temp=data.frame(table(ponds.simple$Sample.Mean))
colnames(temp)=c("Sample.Mean","Frequency")
temp$Relative.Freq= temp$Frequency/sum(temp$Frequency)
temp$Mean.times.Rel.Freq= as.numeric(as.character(temp$Sample.Mean))*temp$Relative.Freq

temp$Sample.Mean=as.character(temp$Sample.Mean)
temp[9,]=c("Sum","15","1","8")

temp$Relative.Freq = as.numeric(temp$Relative.Freq)
temp$Mean.times.Rel.Freq = as.numeric(temp$Mean.times.Rel.Freq)
temp %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"',
        digits = 3) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30)

# temp2=data.frame("SUM",sum(as.numeric(temp$Frequency[1:8])),sum(as.numeric(temp$Relative.Freq[1:8])),sum(as.numeric(temp$Mean.times.Rel.Freq[1:8])))
# colnames(temp2)=NULL
# 
# temp2 %>% 
#   kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
#   kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 40)



## ----echo=FALSE---------------------------------------------------------------
temp=data.frame(table(ponds.simple$Sample.Mean))
colnames(temp)=c("Sample.Mean","Frequency")
temp$Relative.Freq= temp$Frequency/sum(temp$Frequency)
temp$Mean.times.Rel.Freq= as.numeric(as.character(temp$Sample.Mean))*temp$Relative.Freq

hist(ponds.simple$Sample.Mean,freq=TRUE,breaks=10,
     main="Sampling Distribution",
     xlab="Sample Mean")



## ----echo=FALSE---------------------------------------------------------------
hist(ponds.simple$Sample.Mean,freq=TRUE,breaks=10,
     main="Sampling Distribution",
     xlab="Sample Mean")



## ----echo=FALSE---------------------------------------------------------------
hist(ponds.simple$Sample.Mean,freq=TRUE,breaks=10,
     main="Sampling Distribution",
     xlab="Sample Mean")


## ----echo=FALSE---------------------------------------------------------------
ponds.simple %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 20)


## ----echo=TRUE----------------------------------------------------------------
var(ponds.simple$Sample.Mean)


## ----echo=TRUE,results='markup'-----------------------------------------------
(1/(6-1))*sum((ponds$egg.mass-8)^2)


## ----echo=TRUE,results='markup'-----------------------------------------------
var.per.sample = apply(
                       cbind(ponds.all$`First Value`,ponds.all$`Second Value`),
                       1,
                       var
                       )
# Expected value of population variance
  mean(var.per.sample)


## ----echo=FALSE---------------------------------------------------------------
 hist(var.per.sample,xlab="Sample Variance", main="")
 abline(v=mean(var.per.sample),lwd=3)


## ----echo=FALSE---------------------------------------------------------------
par(mfrow=c(2,1))
hist(ponds$egg.mass,freq=TRUE,breaks=10,
     main="",
     xlab="Sample Units (egg masses)",ylim=c(0,3),xlim=c(0,12))
hist(ponds.simple$Sample.Mean,freq=TRUE,breaks=10,
     main="",
     xlab="Sample Mean",ylim=c(0,3),xlim=c(0,12))


## ----fig.width=15-------------------------------------------------------------
#sampling distribution of n=4
  ponds.numbers2=t(utils::combn(1:6,4))
  ponds.all2=matrix(ponds$egg.mass[c(ponds.numbers2)],ncol=4,byrow = FALSE)

  ponds.numbers3=t(utils::combn(1:6,6))
  ponds.all3=matrix(ponds$egg.mass[c(ponds.numbers2)],ncol=5,byrow = FALSE)

  
#expected variance of sampling distributiuon

hist(ponds.simple$Sample.Mean,breaks=10,xlim=c(3,12),ylim=c(0,4),xlab="Sample Means",main="Sampling Distributions \n of the Mean",cex = 2,cex.axis=2,cex.lab=2,cex.main=2)
hist(apply(ponds.all2,1,mean),add=TRUE,col=grDevices::adjustcolor("red",alpha.f = 0.5)  ,breaks=10,cex = 2,cex.axis=2,cex.lab=2)
hist(apply(ponds.all3,1,mean),add=TRUE,col=grDevices::adjustcolor("purple",alpha.f = 0.5)  ,breaks=10,cex = 2,cex.axis=2,cex.lab=2)
legend("topleft",legend=c("n=2","n=4","n=5"),col=c("grey",2,"purple"),lwd=5,cex = 2)
abline(v=8,lty=1,col=1,lwd=4)


## ----echo=FALSE---------------------------------------------------------------
 sample.mean.fn = function(target,n){
                                     mean(
                                         sample(target,n)
                                          )
                                     }

#z = rnorm(100,100,10)

set.seed(34234)
z=(rgamma(1000,1,0.01))

#random sample many means n= 10
n = 10
  mu.hat1 = replicate(20000,
                     sample.mean.fn(z,n)
                     )
n = 20  
  mu.hat2 = replicate(20000,
                     sample.mean.fn(z,n)
                     )
n = 50  
  mu.hat3 = replicate(20000,
                     sample.mean.fn(z,n)
                     )
n = 100    
  mu.hat4 = replicate(20000,
                     sample.mean.fn(z,n)
                     )
  
  
par(mfrow=c(2,2))
hist(mu.hat1,col=2,breaks=10,freq = TRUE,xlab="Sample Means",main="n = 10",xlim=c(30,200))
abline(v=100,col=1,lwd=3)
hist(mu.hat2,col=grDevices::adjustcolor("red",alpha.f = 0.5),add=FALSE,breaks=10,freq = TRUE,xlab="Sample Means",main="n = 20",xlim=c(30,200))
abline(v=100,col=1,lwd=3)
hist(mu.hat3,col=grDevices::adjustcolor("purple",alpha.f = 0.5),add=FALSE,breaks=10,freq = TRUE,xlab="Sample Means",main="n = 50",xlim=c(30,200))
abline(v=100,col=1,lwd=3)
hist(mu.hat4,col=grDevices::adjustcolor("orange",alpha.f = 0.5),add=FALSE,breaks=20,freq = TRUE,xlab="Sample Means",main="n = 100",xlim=c(30,200))
abline(v=100,col=1,lwd=3)


## ----echo=FALSE---------------------------------------------------------------
tau.est = ponds.simple$Sample.Mean * 6
hist(tau.est,main="",xlab="Totals per Sample")
abline(v=sum(ponds$egg.mass),lwd=5,col=1,lty=1)
abline(v=mean(ponds.simple$Sample.Mean) * 6,lwd=5,lty=3,col=3)
legend("topleft",col=c(1,3),legend=c("truth","expectation"),lwd=4)



## ----echo=FALSE---------------------------------------------------------------
hist(ponds.simple$Sample.Mean * 6,main="",xlab="Totals per Sample")


## ----echo=TRUE----------------------------------------------------------------
length(which( tau.est <=  2*48))/length(tau.est)


## ----echo=FALSE, results='hide'-----------------------------------------------
#Output code into R file
knitr::purl(input="../FW552/SRS.qmd",output="../FW552/SRS.r")

