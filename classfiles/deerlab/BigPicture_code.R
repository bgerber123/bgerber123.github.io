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
# Option 1- sample values directly
  n = 10
  y = sample(c(mat),10, replace = TRUE)
  y


## ----design.sim3, echo=TRUE, eval=TRUE----------------------------------------------------------------------------------
# Option 2 - Sample the coordinates and get values

  coords = expand.grid(x = 1:5, y = 1:5)
  index.samples = sample(1:nrow(coords),10)
  y2 = mat[as.matrix(coords[index.samples,])]
  y2 


## ----design.sim4, echo=TRUE, eval=TRUE----------------------------------------------------------------------------------
# Sample mean
  mean(y)
  mean(y2)
  


## ----model.sim, echo=TRUE, eval=TRUE------------------------------------------------------------------------------------
# This is only one realization
  lambda=200
  mat = matrix(rpois(25, lambda=lambda),
               nrow=5, ncol=55
               )

#Create a function 
  n.sim=500
  mat.fn = function(mu){matrix(rpois(25, lambda=lambda),
                               nrow=5, ncol=5
                               )
  }


## ----model.sim2, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------
# repeat the function n.sim times
  list.mat = replicate(n.sim, mat.fn(mu), simplify=FALSE)
  length(list.mat)


## ----model.sim3, echo=TRUE, eval=TRUE-----------------------------------------------------------------------------------
#One realization
  list.mat[[1]]


# Sample mean for the first realization
  mean(list.mat[[1]])


## ----bias1,eval=TRUE, echo=TRUE-----------------------------------------------------------------------------------------
  lambda.hat = unlist(lapply(list.mat,FUN=mean))

  hist(lambda.hat,xlab=bquote(lambda-hat),main=bquote("Sampling Distribution of"~lambda-hat))



## ----bias22,eval=TRUE, echo=TRUE----------------------------------------------------------------------------------------
  # Bias
  bias.lambda = mean(lambda.hat) - lambda
  bias.lambda
  
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

