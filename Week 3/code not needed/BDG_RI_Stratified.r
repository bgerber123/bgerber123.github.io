#My code  

RI3=RI

#Create the deer population
  beta0=log(20)
  beta1=-1
  mean.deer=exp(beta0+beta1*RI3$Devel_area)
  
  set.seed(3243)
  RI3$Deer=rpois(nrow(RI3),mean.deer)
  
  plot(RI3["Deer"])
  hist(RI3$Deer)
  true.total2=sum(RI3$Deer)

  sample.sizes=c(5, 10, 20, 40)
  n.sim=100
  
  #Register how many processors to use (backend setup)
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  #Start timer and foreach parallel loop with "dopar"  
  tic("Simulation Parallel")
  #Define the ouput of the foreach loop
  out.parallel<-foreach(z=1:length(sample.sizes),
                        .packages = c("spsurvey"))%dopar% {
                          
                          deer.total.abundance=rep(0,n.sim)
                          for(i in 1:n.sim){  
                            #Sample/model/predict deer
                            eqprob <- grts(RI3, n_base = sample.sizes[z])
                            y=eqprob$sites_base$Deer
                            model1=glm(y~1,family=poisson(link="log"))
                            deer.total.abundance[i]=predict(model1,type="response")[1]*nrow(RI)
                          }
                          #Output this object to the object out.parallel
                          deer.total.abundance
                          
                        } #End foreach loop
  toc() #End timer
  
  #stop procesors
  stopCluster(cl)
  
  #Examine results  
  save(out.parallel,file="out.parallel.2")
  

  par(mfrow=c(2,2))
  hist(out.parallel[[1]],main="Strata Sample.Size = 2",xlim=c(8000,20000),xlab="Total Abundance")
  abline(v=true.total2,col=2,lwd=3)
  
  hist(out.parallel[[2]],main="Strata Sample.Size = 4",xlim=c(8000,20000),xlab="Total Abundance")
  abline(v=true.total2,col=2,lwd=3)
  
  hist(out.parallel[[3]],main="Strata Sample.Size = 8",xlim=c(8000,20000),xlab="Total Abundance")
  abline(v=true.total2,col=2,lwd=3)
  
  hist(out.parallel[[4]],main="Strata Sample.Size = 16",xlim=c(8000,20000),xlab="Total Abundance")
  abline(v=true.total2,col=2,lwd=3)
  
  
  length(which(abs(out.parallel[[1]]-true.total)<2000))/length(out.parallel[[1]])
  length(which(abs(out.parallel[[2]]-true.total)<2000))/length(out.parallel[[1]])
  length(which(abs(out.parallel[[3]]-true.total)<2000))/length(out.parallel[[3]])
  length(which(abs(out.parallel[[4]]-true.total)<2000))/length(out.parallel[[4]])
  
  
#####################################
#####################################
#NOT NEEDED  
  
numbers_of_bins = 3
MyQuantileBins = cut(RI3$Devel_area,  breaks = unique(quantile(RI3$Devel_area,probs=seq.int(0,1, by=1/numbers_of_bins))),include.lowest=TRUE)
MyQuantileBins
levels(MyQuantileBins)=c("low","med","high")

RI3$strata = MyQuantileBins

plot(RI3["strata"])

sample.size=c(10,20,40,80)
n.sim=100

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

#for(z in 1:length(sample.sizes)){
out.parallel2<-foreach(z=1:length(sample.sizes),.inorder=TRUE,
                      .packages = c("spsurvey"))%dopar% {

deer.total.abundance=rep(0,n.sim)
n.size=
n.size.input=c(low=n.sample.size[z],
               med=n.sample.size[z],
               high=n.sample.size[z])

for(i in 1:n.sim){  
#Sample deer
  eqprob <- grts(shp3, n_base = n.size.input, mindis = 3218.69,seltype = "equal",
                 stratum_var="strata")
  #sp_plot(shp3,formula = ~strata,pch=18,cex=2)
  #sp_plot(eqprob, shp3)
  y=eqprob$sites_base$Deer
  x=matrix(shp3$Devel_area[eqprob$sites_base$ID3])
  
  dat=data.frame(y=y,x=x)
  model1=glm(y~x,family=poisson(link="log"),data=dat)
  x.mat=data.frame(x=matrix(shp3$Devel_area))
  preds=predict(model1,newdata =x.mat ,type="response")
  deer.total.abundance[i]=sum(preds)
#  if(i%%100==0) cat(i," ")
}
deer.total.abundance
#print("z = ", z)
                      }
#stop cluster
stopCluster(cl)

out.parallel2

save(out.parallel2,file="out.parallel.2")
length(out.parallel2)

par(mfrow=c(2,2))
hist(out.parallel2[[1]],main="Strata Sample.Size = 2",xlim=c(8000,20000),xlab="Total Abundance")
abline(v=true.total2,col=2,lwd=3)

hist(out.parallel2[[2]],main="Strata Sample.Size = 4",xlim=c(8000,20000),xlab="Total Abundance")
abline(v=true.total2,col=2,lwd=3)

hist(out.parallel2[[3]],main="Strata Sample.Size = 8",xlim=c(8000,20000),xlab="Total Abundance")
abline(v=true.total2,col=2,lwd=3)

hist(out.parallel2[[4]],main="Strata Sample.Size = 16",xlim=c(8000,20000),xlab="Total Abundance")
abline(v=true.total2,col=2,lwd=3)


length(which(abs(out.parallel2[[1]]-true.total)<2000))/length(out.parallel2[[1]])
length(which(abs(out.parallel2[[2]]-true.total)<2000))/length(out.parallel2[[1]])
length(which(abs(out.parallel2[[3]]-true.total)<2000))/length(out.parallel2[[3]])
length(which(abs(out.parallel2[[4]]-true.total)<2000))/length(out.parallel2[[4]])

p=plogis(rnorm(length(out.parallel[[1]]),qlogis(0.9),0.5))

par(mfrow=c(2,2))
hist(out.parallel2[[1]]*p,main="Sample.Size = 5",xlim=c(8000,20000),xlab="Total Abundance")
abline(v=true.total,col=2,lwd=3)

hist(out.parallel2[[2]]*p,main="Sample.Size = 10",xlim=c(8000,20000),xlab="Total Abundance")
abline(v=true.total,col=2,lwd=3)

hist(out.parallel2[[3]]*p,main="Sample.Size = 20",xlim=c(8000,20000),xlab="Total Abundance")
abline(v=true.total,col=2,lwd=3)

hist(out.parallel2[[4]]*p,main="Sample.Size = 40",xlim=c(8000,20000),xlab="Total Abundance")
abline(v=true.total,col=2,lwd=3)


##############################################
