## ----design.sim, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------
#random discrete uniform sampler
rdu<-function(n,lower,upper){sample(lower:upper,n,replace=T)}

mat = matrix(rdu(25, 
                 lower = 0, 
                 upper = 400
                 ),
             nrow=5, ncol=5
             )
mat


## ----design.sim2, echo=TRUE, eval=TRUE----------------------------------------------------------------------------------
  n = 10
  y = sample(
             c(mat),
             10, 
             replace = TRUE
             )
  y


## ----design.sim4, echo=TRUE, eval=TRUE----------------------------------------------------------------------------------
  mean(y)



## ----sampling dist, eval=TRUE, echo=TRUE--------------------------------------------------------------------------------
# How many ways can we uniquely sample 10 things from 25
combs = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}

combs(25, 10)


## ----sampling dist2, eval=TRUE, echo=TRUE, cache=TRUE-------------------------------------------------------------------
  set.seed(5435)
  all.combs = utils::combn(c(mat), 10)
  dim(all.combs)
  mean.all.combs = apply(all.combs,2,mean)


## ----sampling dist3, eval=TRUE, echo=TRUE, cache=TRUE-------------------------------------------------------------------
  set.seed(5435)
  sim.sampling.dist=replicate(2000,
                              sample(c(mat),10)
                              )
  dim(sim.sampling.dist)
  mean.samples = apply(sim.sampling.dist,2,mean)


## ----sampling dist4, eval=TRUE, echo=FALSE------------------------------------------------------------------------------
  hist(mean.all.combs,
       freq=FALSE, 
       main="Sampling Distributiuon of the Mean",
       breaks=10,
       xlab="Means"
       )
  hist(mean.samples,
       add=TRUE,
       freq=FALSE,
       col=grDevices::adjustcolor("red", alpha.f=0.5),
       breaks=10
       )
  legend("topright",lwd=5,col=c(grDevices::adjustcolor("black", alpha.f=0.5),grDevices::adjustcolor("red", alpha.f=0.5)),legend = c("Complete", "Approximated"))


## ----model.sim, echo=TRUE, eval=TRUE------------------------------------------------------------------------------------
#Create a function, to be replicated
  lambda=200
  n.sim=500
  mat.fn = function(lambda){matrix(rpois(25, lambda=lambda),
                               nrow=5, ncol=5
                               )
  }


## ----model.sim2, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------
# repeat the function n.sim times
  list.mat = replicate(n.sim, 
                       mat.fn(lambda), 
                       simplify=FALSE
                       )
  length(list.mat)


## ----model.sim3, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------
#One realization
  list.mat[[1]]


# Sample mean for the first realization
  mean(list.mat[[1]])


## ----model.sim4, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------
lambda.hat = unlist(lapply(list.mat,FUN=mean))


## ----model.sim5, echo=FALSE, eval=TRUE----------------------------------------------------------------------------------
hist(lambda.hat,
     xlab=bquote(lambda-hat),
     main=bquote("Sampling Distribution of"~lambda-hat)
     )


## ----bias22,eval=TRUE, echo=TRUE----------------------------------------------------------------------------------------
# Bias
  mean(lambda.hat) - lambda

# relative bias
  (mean(lambda.hat) - lambda)/lambda


## ----bias3,eval=TRUE, echo=FALSE----------------------------------------------------------------------------------------
  hist(lambda.hat,xlab=bquote(lambda), breaks=20)
  abline(v=mean(lambda.hat),lwd=4,col=4,lty=2)
  abline(v=lambda,lwd=4,col=3,lty=3)
  legend("topright",legend=c("mean","truth"),lwd=3,col=c(4,3),lty=c(2,3))


## ----bias4,eval=TRUE, echo=TRUE-----------------------------------------------------------------------------------------
  diff = 0.025*lambda
  diff
  lower = lambda-diff
  upper = lambda+diff

  index = which(lambda.hat>=lower & lambda.hat <= upper)

#Probability of getting a mean within 5% of the truth
  length(index)/n.sim

