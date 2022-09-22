
#Simulate values
  y= matrix(rgamma(1000*200,1,1),nrow=1000)

#Apply function - range on columns
  apply(y,1,FUN=range)
  
  apply(y,2,FUN=sd)
  
#Create own function
  myfun=function(x){max(x)-min(x)}
  hist(apply(y,1,FUN=myfun))
  
#Create for loop    
  save=rep(0,nrow(y))  
  for(i in 1:nrow(y)){
    save[i]=mean(y[i,])
  }
  hist(save)

  hist(apply(y,1,mean))
  
# Create 
  mu=seq(0,1000,250)
  sigma=c(10,1000)
  combs=expand.grid(mu,sigma)
  
  myfunc2=function(a,b){list(rnorm(1000,mean=a,sd=b))}
  
  out=mapply(a=combs$Var1,b=combs$Var2,FUN=myfunc2)
  str(out)
    
#lapply
  this= lapply(out,FUN=function(x){list(mean=mean(x),sd=sd(x))})

  result=matrix(unlist(this),ncol=2,byrow=TRUE)
