## ----knitr, echo=FALSE,results='hide'--------------------------------------------------------------------------------------
#knitr::purl(input="../FW552/Proportions.qmd",output="../FW552/Proportions.r")


## ----echo=FALSE------------------------------------------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 8
x <- seq(1,10,by=1)
y <-  seq(1,10,by=1)

par(mfrow=c(2,2),cex.axis=1.5,cex.lab=2,cex.main=3)
plot(x,y, pch = 19, col = 0,main="2x2",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 2, ny = 2,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)
plot(x,y, pch = 19, col = 0,main="4x4",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 4, ny = 4,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)

plot(x,y, pch = 19, col = 0,main="6x6",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 6, ny = 6,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)

plot(x,y, pch = 19, col = 0,main="8x8",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 8, ny = 8,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)


## ----echo=FALSE------------------------------------------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 8
x <- seq(1,10,by=1)
y <-  seq(1,10,by=1)

par(mfrow=c(2,2),cex.axis=1.5,cex.lab=2,cex.main=3)
plot(x,y, pch = 19, col = 0,main="2x2",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 2, ny = 2,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)
#text(7,5,bquote(sigma^2==.(round(0.5*(1-0.5)/(4-1),digits=2))),col="purple",cex=2)
text(6.5,3.5,bquote(p==.(0.5,digits=2)),col="purple",cex=2)

plot(x,y, pch = 19, col = 0,main="4x4",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 4, ny = 4,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)
#text(7,5,bquote(sigma^2==.(round(0.3125*(1-0.3125)/(16-1),digits=4))),col="purple",cex=2)
text(6.5,3.5,bquote(p==.(round(5/16,digits=2))),col="purple",cex=2)

plot(x,y, pch = 19, col = 0,main="6x6",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 6, ny = 6,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)
#text(7,5,bquote(sigma^2==.(round(0.1388*(1-0.1388)/(36-1),digits=4))),col="purple",cex=2)
text(6.5,3.5,bquote(p==.(round(5/36,digits=2))),col="purple",cex=2)


plot(x,y, pch = 19, col = 0,main="8x8",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 8, ny = 8,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)
#text(7,5,bquote(sigma^2==.(round(0.078125*(1-0.078125)/(64-1),digits=4))),col="purple",cex=2)
text(6.5,3.5,bquote(p==.(round(5/64,digits=2))),col="purple",cex=2)


## ----echo=FALSE------------------------------------------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 8
x <- seq(1,10,by=1)
y <-  seq(1,10,by=1)

par(mfrow=c(2,2),cex.axis=1.5,cex.lab=2,cex.main=3)
plot(x,y, pch = 19, col = 0,main="2x2",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 2, ny = 2,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)
text(7,5,bquote(sigma^2==.(round(0.5*(1-0.5)/(4-1),digits=2))),col="purple",cex=2)
text(6.5,3.5,bquote(p==.(0.5,digits=2)),col="purple",cex=2)

plot(x,y, pch = 19, col = 0,main="4x4",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 4, ny = 4,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)
text(7,5,bquote(sigma^2==.(round(0.3125*(1-0.3125)/(16-1),digits=4))),col="purple",cex=2)
text(6.5,3.5,bquote(p==.(round(5/16,digits=2))),col="purple",cex=2)

plot(x,y, pch = 19, col = 0,main="6x6",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 6, ny = 6,
     lty = 2,      
     col = "gray", 
     lwd = 2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)
text(7,5,bquote(sigma^2==.(round(0.1388*(1-0.1388)/(36-1),digits=4))),col="purple",cex=2)
text(6.5,3.5,bquote(p==.(round(5/36,digits=2))),col="purple",cex=2)


plot(x,y, pch = 19, col = 0,main="8x8",xaxt="n",yaxt="n",xlab="",ylab="")
grid(nx = 8, ny = 8,
     lty = 2,      
     col = "gray", 
     lwd = 2)
text(7,5,bquote(sigma^2==.(round(0.078125*(1-0.078125)/(64-1),digits=4))),col="purple",cex=2)
points(x=c(1,4,5,2,2),y=c(1,5,7,4,9),pch=16,col=2,cex=2)
text(6.5,3.5,bquote(p==.(round(5/64,digits=2))),col="purple",cex=2)


## ----echo=FALSE------------------------------------------------------------------------------------------------------------
n=10
N=100
p=seq(0,1,by=0.1)
var.p = ((p*(1-p)))/n
plot(p,var.p,type="b",col="purple",lwd=3,ylab="VAR(p)") 
 


## ----echo=FALSE------------------------------------------------------------------------------------------------------------
n=10
N=100
p=seq(0,1,by=0.1)
var.p = ((p*(1-p)))/n
plot(p,sqrt(var.p),type="b",col="purple",lwd=3,ylab="SD(p)") 
 


## ----echo=FALSE------------------------------------------------------------------------------------------------------------

curve(dnorm(x,0.5,0.15),xlim=c(0,1),xlab="Proportion",col=1,ylim=c(0,20))
these=rev(seq(0.01,0.15,by=0.01))
cols=viridis::viridis(15)
for(i in 1:15){
   curve(dnorm(x,0.5,these[i]),col=cols[i],add=TRUE,lwd=3)
}
legend("topright",lwd=3,col=cols[c(1,5,10,15)],legend=c(these[c(1,5,10,15)]))



## ----echo=FALSE------------------------------------------------------------------------------------------------------------

n = seq(2,1000)
p = seq(0,1,by=0.01)
        
all.combs=expand.grid(n,p)
      
p.var.fn=function(x){(x[2]*(1-x[2]))/(x[1]-1)}
p.var=apply(all.combs,1,p.var.fn)
p.sd=sqrt(p.var)

dat.plot=data.frame(n=all.combs[,1],
                    p=all.combs[,2],
                    sd = p.sd)



## ----echo=FALSE, fig.height=6, fig.width=10--------------------------------------------------------------------------------
library(plotly)
fig <- plot_ly(dat.plot, x = ~n, y = ~p, z = ~sd, marker = list(size = 5))
fig <- fig %>% add_markers(color=~n)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Sample Size'),
                     yaxis = list(title = 'Proportion'),
                     zaxis = list(title = 'SD(p)')))
fig



## ----echo=FALSE------------------------------------------------------------------------------------------------------------
#Lets say there are 600 sites
  N = 600
  prob.occ=0.5
  n.occu=prob.occ*N
  sites=1:N

# Decide on a fixed number of sites that are occupied wiht
# expect probability prob.occ
  set.seed(5454)
  occ.sites = sample(sites,n.occu,replace=FALSE)
  
  
# Crate dataframe of samples sites
  truth = data.frame(sites=sites,
                    occu = 0)
  truth$occu[occ.sites]=1
  head(truth)


## ----setup,echo=FALSE------------------------------------------------------------------------------------------------------
# Probability of sampling an occupied cell
  truth$prob.sample=1/N

#Now simulate many samples and get proportion of occupied sites
# How many sites will we sample  
  n=200
  
#how many simulations
  n.sim=4000

probs.save=rep(NA, n.sim)

for(i in 1:n.sim){
  sample.sites=sample(truth$sites,
                     n,
                     prob = truth$prob.sample
                     )
  sampled.occu=truth$occu[sample.sites]
  probs.save[i]=mean(sampled.occu)

} 

#Visualize sampling distribution of proportion sites occupied
#And line for true value
  hist(probs.save,breaks=10,xlim=c(0.3,0.7),freq=FALSE,ylim=c(0,20),main="")
  abline(v=prob.occ,col=2,lwd=3)
  
  
# Statistical Bias
 # mean(probs.save)-prob.occ


## ----ancillary,echo=FALSE--------------------------------------------------------------------------------------------------
probs.save2=rep(NA, n.sim)

for(i in 1:n.sim){
  sample.sites=sample(truth$sites,
                     n,
                     prob = truth$prob.sample
                     )
 
  
#which sites have occurences
  these=which(truth$occu==1)
  ancillary.sites=sample(these[-sample.sites],10)
   
  sampled.occu=truth$occu[c(sample.sites,ancillary.sites)]
  
  probs.save2[i]=mean(sampled.occu)
  
} 

#Visualize sampling distribution of proportion sites occupied
#And line for true value
  hist(probs.save,breaks=10,main="SRS + 10 sites",xlim=c(0.3,0.7),freq=FALSE,ylim=c(0,20))
  hist(probs.save2,breaks=10,col=grDevices::adjustcolor("green",alpha.f=0.5),add=TRUE,freq=FALSE)
  abline(v=prob.occ,col=2,lwd=3)



## ----increase.ancillary,echo=FALSE-----------------------------------------------------------------------------------------
probs.save3=rep(NA, n.sim)

for(i in 1:n.sim){
  sample.sites=sample(truth$sites,
                     n,
                     prob = truth$prob.sample
                     )
 
  
#which sites have occurences
  these=which(truth$occu==1)
  ancillary.sites=sample(these[-sample.sites],20)
   
 sampled.occu=truth$occu[c(sample.sites,ancillary.sites)]
  
  probs.save3[i]=mean(sampled.occu)
  
} 

#Visualize sampling distribution of proportion sites occupied
#And line for true value
  hist(probs.save,breaks=10,main="SRS + 20 sites",xlim=c(0.3,0.7),freq=FALSE,ylim=c(0,20))
  hist(probs.save3,breaks=10,col=grDevices::adjustcolor("green",alpha.f=0.5),add=TRUE,freq=FALSE)
  abline(v=prob.occ,col=2,lwd=3)


## ----unequal,echo=FALSE,results='hide'-------------------------------------------------------------------------------------
# per unit probability of sampling a site that has an occurrence
  p.sample.occ=1.333333/N
  
# per unit probability of sampling a site that has no occurrence
  p.sampling.unocc = (1-(p.sample.occ*n.occu))/(N-n.occu)
  
# overall probability of selecting an occupied site
  p.sample.occ*n.occu
  
# overall probability of selecting an unoccupied site
  p.sampling.unocc*(N-n.occu)
  
# 2x higher prob of selection of a site that is occupied
  p.sample.occ/p.sampling.unocc
  
  truth$prob.sample=truth$occu
  truth$prob.sample[which(truth$occu==1)]=p.sample.occ
  truth$prob.sample[which(truth$occu==0)]=p.sampling.unocc
  
  #create columns of ratio of weights
#  truth$prob.sample.ratio=truth$prob.sample
#  truth$prob.sample.ratio[which(as.character(truth$prob.sample.ratio)=="0.003333335")]=1
#  truth$prob.sample.ratio[which(as.character(truth$prob.sample.ratio)=="0.006666665")]= 3
  
#  head(truth)
  
#Setup for loop simulation  
  probs.save4=rep(NA, n.sim)
  probs.est4=rep(NA, n.sim)
  probs.est.model=rep(NA, n.sim)
  for(i in 1:n.sim){
    sample.sites=sample(truth$sites,n,replace = FALSE,
                        prob=truth$prob.sample)
    data = truth[sample.sites,]
    probs.save4[i] = mean(data$occu)
    
     # options(warn=-1)
     # model=glm(occu~1,family=binomial,data=data,
     #           weights=prob.sample.ratio)
     # options(warn=0)
     # probs.est.model[i]=predict(model,type="response")[1]
    
    # Generalized Unequal-Probability Estimator
    probs.est4[i] = sum(data$occu/data$prob.sample) /sum((1/data$prob.sample))
  }


## ----hist,echo=FALSE-------------------------------------------------------------------------------------------------------
hist(probs.save4,freq = FALSE,xlim=c(0.3,1),main="")
abline(v=prob.occ,col=2,lwd=3)


## ----hist2,echo=FALSE------------------------------------------------------------------------------------------------------
hist(probs.est4,freq = FALSE,xlim=c(0.3,1),main="")
abline(v=prob.occ,col=2,lwd=3)

