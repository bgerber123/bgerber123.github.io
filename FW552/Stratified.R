## ----echo=FALSE,warning=FALSE,results='hide'------------------------------------------------------------------------------------------------
library(tidyverse)
library(kableExtra)
library(magrittr)
library(knitr)


## ----knitr, echo=FALSE,results='hide'-------------------------------------------------------------------------------------------------------
#knitr::purl(input="../FW552/Stratified.qmd",output="../FW552/Stratified.R")


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------
ponds = data.frame(matrix(nrow=6,ncol=2))
colnames(ponds)=c("Pond","egg.mass")

N = 6
Nh = 3
nh= 2
mu = 8

ponds$Pond=LETTERS[1:6]
ponds$egg.mass = c(2,6,8,10,10,12)
ponds$strata = c(1,1,1,2,2,2)

ponds %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 40)



## ----echo=TRUE------------------------------------------------------------------------------------------------------------------------------
choose(3,2)


## ----echo=TRUE------------------------------------------------------------------------------------------------------------------------------
choose(3,2)


## ----echo=TRUE------------------------------------------------------------------------------------------------------------------------------
choose(3,2)*choose(3,2)


## -------------------------------------------------------------------------------------------------------------------------------------------
letters.S1=t(utils::combn(LETTERS[c(1,2,3)],2))
letters.S2=t(utils::combn(LETTERS[c(4,5,6)],2))

num.S1=t(utils::combn(c(1,2,3),2))
num.S2=t(utils::combn(c(4,5,6),2))

first.strata = rbind(
                matrix(rep(letters.S1[1,],3),nrow=3,byrow = TRUE),
                matrix(rep(letters.S1[2,],3),nrow=3,byrow = TRUE),
                matrix(rep(letters.S1[3,],3),nrow=3,byrow = TRUE)
)

first.strata.num = rbind(
                matrix(rep(num.S1[1,],3),nrow=3,byrow = TRUE),
                matrix(rep(num.S1[2,],3),nrow=3,byrow = TRUE),
                matrix(rep(num.S1[3,],3),nrow=3,byrow = TRUE)
)


sec.strata=rbind(letters.S2,letters.S2,letters.S2)
sec.strata.num=rbind(num.S2,num.S2,num.S2)

comb.strats=data.frame(cbind(first.strata,sec.strata))
comb.strats.num=cbind(first.strata.num,sec.strata.num)

colnames(comb.strats)=c("S1.1","S1.2","S2.1","S2.2")           

strata.1.eggs=cbind(ponds$egg.mass[comb.strats.num[,1]],
                    ponds$egg.mass[comb.strats.num[,2]]
                    )
strata.2.eggs=cbind(ponds$egg.mass[comb.strats.num[,3]],
                    ponds$egg.mass[comb.strats.num[,4]]
                    )

strata.1.means = apply(strata.1.eggs,1,mean)
strata.2.means = apply(strata.2.eggs,1,mean)
strata.1.var = apply(strata.1.eggs,1,var)
strata.2.var = apply(strata.2.eggs,1,var)


comb.strats=data.frame(Sample = 1:9,
                       comb.strats,
                        Mean.S1 = strata.1.means,
                        Mean.S2 = strata.2.means,
                        Var.S1 = strata.1.var,
                        Var.S2 = strata.2.var
                       )


comb.strats %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"',align = 'c') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30, position = "center")



## -------------------------------------------------------------------------------------------------------------------------------------------

s2.h=cbind(strata.1.var,strata.2.var)
# calculate sampling variance

s2 = (Nh/N)^2 * ((Nh-nh)/(Nh)) * (s2.h/nh)
s2=apply(s2,1,sum)
comb.strats=comb.strats[,-1]

comb.strats$Var.mean = round(s2,digits=2)

comb.strats %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"',align = 'c') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30, position = "center")



## -------------------------------------------------------------------------------------------------------------------------------------------

# Stratified RS population-level mean
pop.mean.stratified = (strata.1.means*Nh)/N + (strata.2.means*Nh)/N


# Get sample mean distribution under n=2 and n=4 for SRS
ponds.numbers=utils::combn(1:6,2)
mean.SRS.n2 = apply(matrix(ponds$egg.mass[c(ponds.numbers)],ncol=2,byrow = TRUE),1,mean)

ponds.numbers=utils::combn(1:6,4)
mean.SRS.n4 = apply(matrix(ponds$egg.mass[c(ponds.numbers)],ncol=4,byrow = TRUE),1,mean)

hist(mean.SRS.n2,breaks=10,main="Sampling Distibution of Mean",freq=TRUE,ylim=c(0,10),xlim=c(3,12),xlab="Sample Mean")
abline(v=mu,lwd=3,col=1)
legend("topleft",lwd=3,col=c(1,2),legend=c("SRS n=2; comb = 15"))



## -------------------------------------------------------------------------------------------------------------------------------------------
hist(mean.SRS.n2,breaks=10,main="Sampling Distibution of Mean",freq=TRUE,ylim=c(0,10),xlim=c(3,12),xlab="Sample Mean")
hist(mean.SRS.n4,breaks=10,add=TRUE,
     col=grDevices::adjustcolor("red",alpha.f=0.5))
abline(v=mu,lwd=3,col=1)
legend("topleft",lwd=3,col=c(1,2),legend=c("SRS n=2 ; comb = 15", "SRS n=4; comb = 15"))


## -------------------------------------------------------------------------------------------------------------------------------------------
hist(mean.SRS.n2,breaks=10,main="Sampling Distibution of Mean",freq=TRUE,ylim=c(0,10),xlim=c(3,12),xlab="Sample Mean")
hist(mean.SRS.n4,breaks=10,add=TRUE,
     col=grDevices::adjustcolor("red",alpha.f=0.5))
hist(pop.mean.stratified,breaks=5,add=TRUE,
     col=grDevices::adjustcolor("purple",alpha.f=0.5))
abline(v=mu,lwd=3,col=1)
legend("topleft",lwd=3,col=c(1,2,"purple"),legend=c("SRS n=2; comb = 15", "SRS n=4; comb = 15","Strat n=4; comb = 9"))


## -------------------------------------------------------------------------------------------------------------------------------------------
hist(mean.SRS.n2,breaks=10,main="Sampling Distibution of Mean",freq=TRUE,ylim=c(0,10),xlim=c(3,12),xlab="Sample Mean")
hist(mean.SRS.n4,breaks=10,add=TRUE,
     col=grDevices::adjustcolor("red",alpha.f=0.5))
hist(pop.mean.stratified,breaks=5,add=TRUE,
     col=grDevices::adjustcolor("purple",alpha.f=0.5))
abline(v=mu,lwd=3,col=1)
legend("topleft",lwd=3,col=c(1,2,"purple"),legend=c("SRS n=2; comb = 15", "SRS n=4; comb = 15","Strat n=4; comb = 9"))

text(7,6,"ALL UNBIASED ESTIMATORS",cex=3,col=1)


## -------------------------------------------------------------------------------------------------------------------------------------------
ponds %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 40) %>% column_spec(3, background = c("purple", "purple", "purple", "lavender","lavender","lavender"))


## -------------------------------------------------------------------------------------------------------------------------------------------
letters.S1=t(utils::combn(LETTERS[c(1,2,3)],3))
letters.S2=t(utils::combn(LETTERS[c(4,5,6)],1))

num.S1=t(utils::combn(c(1,2,3),3))
num.S2=t(utils::combn(c(4,5,6),1))

first.strata = rbind(letters.S1,letters.S1,letters.S1)

first.strata.num = rbind(num.S1,num.S1,num.S1)


sec.strata=rbind(letters.S2)
sec.strata.num=rbind(num.S2)

comb.strats=data.frame(cbind(first.strata,sec.strata))
comb.strats.num=cbind(first.strata.num,sec.strata.num)

colnames(comb.strats)=c("S1.1","S1.2","S1.3","S2.1")           

strata.1.eggs=ponds$egg.mass[comb.strats.num[1,1:3]]
strata.1.mean=mean(strata.1.eggs)

#values and means are the same
strata.2.eggs=ponds$egg.mass[comb.strats.num[,4]]

#Get population mean
#3 = n_h for stratum sample size

pop.means=(rep(strata.1.mean,3)*3 + strata.2.eggs*3) * 1/N

#strata.1.var = rep(var(strata.1.eggs),3)
#strata.2.var = rep(0,3)

#strata.var = (3/6)^2 * ((3-3) / 3) *  (strata.1.var / 3) + (1/6)^2 * #((3-1) / 3)*  (strata.2.var / 1)

comb.strats2=data.frame(Sample = 1:3,
                        comb.strats,
                        Mean.S1 = rep(strata.1.mean,3),
                        Mean.S2 = strata.2.eggs,
                        pop.means=pop.means
                        
                       )


comb.strats2 %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"',align = 'c') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30, position = "center")



## -------------------------------------------------------------------------------------------------------------------------------------------
ponds %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 40) %>% column_spec(3, background = c("lavender", "lavender", "lavender", "purple","purple","purple"))


## -------------------------------------------------------------------------------------------------------------------------------------------
letters.S1=t(utils::combn(LETTERS[c(1,2,3)],1))
letters.S2=t(utils::combn(LETTERS[c(4,5,6)],3))

num.S1=t(utils::combn(c(1,2,3),1))
num.S2=t(utils::combn(c(4,5,6),3))

sec.strata = rbind(letters.S2,letters.S2,letters.S2)

sec.strata.num = rbind(num.S2,num.S2,num.S2)


first.strata=rbind(letters.S1)
first.strata.num=rbind(num.S1)

comb.strats=data.frame(cbind(first.strata,sec.strata))
comb.strats.num=cbind(first.strata.num,sec.strata.num)

colnames(comb.strats)=c("S1.1","S2.1","S2.2","S2.3")           

strata.2.eggs=ponds$egg.mass[comb.strats.num[1,2:4]]
strata.2.mean=mean(strata.2.eggs)


strata.1.eggs=ponds$egg.mass[comb.strats.num[,1]]

#Get population mean
#3 = n_h for stratum sample size

pop.means=(rep(strata.2.mean,3)*3 + strata.1.eggs*3) * 1/N

#strata.1.var = rep(var(strata.1.eggs),3)
#strata.2.var = rep(0,3)

#strata.var = (3/6)^2 * ((3-3) / 3) *  (strata.1.var / 3) + (1/6)^2 * #((3-1) / 3)*  (strata.2.var / 1)

comb.strats2=data.frame(Sample = 1:3,
                        comb.strats,
                        Mean.S1 = strata.1.eggs,
                        Mean.S2 = strata.2.mean,
                        pop.means=pop.means
                        
                       )


comb.strats2 %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"',align = 'c') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30, position = "center")


## ----echo=FALSE-----------------------------------------------------------------------------------------------------------------------------
one.sample=c(strata.1.eggs[1],strata.2.eggs)
two.sample=c(strata.1.eggs[2],strata.2.eggs)
three.sample=c(strata.1.eggs[3],strata.2.eggs)

#mean(one.sample)
#mean(two.sample)
#mean(three.sample)

biased.estimator=data.frame(S1 = one.sample,
                            S2 = two.sample,
                            S3 = three.sample
                            )
biased.estimator %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"',align = 'c') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30, position = "center")



## -------------------------------------------------------------------------------------------------------------------------------------------
ponds %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 40) %>% column_spec(3, background = c("lavender", "lavender", "lavender", "purple","purple","purple"))


## -------------------------------------------------------------------------------------------------------------------------------------------
biased.estimator=data.frame(Strata = c(1,2,2,2),
                            S1 = one.sample,
                            S2 = two.sample,
                            S3 = three.sample,
                            Weight = c(1,1/3,1/3,1/3)
                            )
biased.estimator %>% 
  kable(table.attr = 'data-quarto-disable-processing="true"',align = 'c') %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped"), full_width = FALSE,font_size = 30, position = "center")


