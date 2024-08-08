## ----eval=TRUE,echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
# A data point
  y = c(10)

#the likelihood the mean is 8, given our data
  dnorm(y, mean = 8)


## ----eval=TRUE,echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
#the likelihood the mean is 9, given our data
  dnorm(y, mean = 9)


## ----eval=TRUE,echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
  means = seq(0, 20,by = 0.1) # many guesses of the mean
  likelihood = dnorm(y, mean = means, sd = 1) # likelihood of each guess of the mean


## ----eval=TRUE,echo=FALSE, fig.align='center'-------------------------------------------------------------------------------------------------------------
#Look at guesses and likelihood
  plot(means,likelihood,xlab="Guesses for the Mean")
  abline(v=10,lwd=3,col=3)
  legend("topright",legend=c("Max Likelihood"),
         lwd=3,col=3)


## ----echo=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
# penguin height data
  y = c(4.34, 3.53, 3.75)

# Joint likelihood of mu=3, sigma =1, given our data
  prod(dnorm(y, mean = 3,sd = 1))


## ----echo=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
# The Guesses
  mu = seq(0,6,0.05)
  sigma = seq(0.01,2,0.05)
  try = expand.grid(mu,sigma)
  colnames(try) = c("mu","sigma")

# function
fun = function(a,b){
          prod(dnorm(y,mean = a, sd = b))
      }

# mapply the function with the inputs
  likelihood = mapply(a = try$mu, b = try$sigma, FUN=fun)


## ----echo=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
# maximum likelihood of parameters
  try[which.max(likelihood),]


## ----echo=TRUE, eval=TRUE---------------------------------------------------------------------------------------------------------------------------------
sum(y)/length(y)


## ----echo=FALSE,eval=TRUE, out.width="150%"---------------------------------------------------------------------------------------------------------------
library(plotly)
f <- list(
    size = 15,
    family = 'sans-serif'
  )
  m <- list(
    l = 2,
    r = 0,
    b = 2,
    t = 2,
    pad = 0
  )
all=cbind(try,likelihood)
colnames(all)=c("mu","sigma","likelihood")
fig <- plot_ly(all, x = ~mu, y = ~sigma, z = ~likelihood, marker = list(size = 5),width = 800, height = 800)
fig <- fig %>% add_markers(color=~likelihood)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'mu'),
                     yaxis = list(title = 'sigma'),
                     zaxis = list(title = 'Likelihood')))
fig %>% layout(font = f, margin = m)

#fig


## ----eval=TRUE,echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
set.seed(154541)
y=rnorm(100,3.8,1)

try=expand.grid(seq(0,6,0.01),seq(0.01,2,0.01))
  colnames(try)=c("mu","sigma")

# mapply the function with the inputs
  likelihood=mapply(try$mu,try$sigma, FUN=fun)

  library(plotly)
f <- list(
    size = 15,
    family = 'sans-serif'
  )
  m <- list(
    l = 2,
    r = 0,
    b = 2,
    t = 2,
    pad = 0
  )
all=cbind(try,likelihood)
colnames(all)=c("mu","sigma","likelihood")
fig <- plot_ly(all, x = ~mu, y = ~sigma, z = ~likelihood, marker = list(size = 5),width = 800, height = 800)
fig <- fig %>% add_markers(color=~likelihood)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Mean'),
                     yaxis = list(title = 'SD'),
                     zaxis = list(title = 'Likelihood')))
fig %>% layout(font = f, margin = m)

  


## ----eval=TRUE,echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
fun.log = function(a,b){
              sum(dnorm(y,mean = a, sd = b, log=TRUE))
          }

log.likelihood = mapply(a = try$mu, b = try$sigma, FUN=fun.log)
  
# maximum log-likelihood of parameters
  try[which.max(log.likelihood),]



## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------

# Note: optim function uses minimization, not maximization. 
# WE want to find the minimum negative log-likelihood
# THUS, need to put negative in our function

neg.log.likelihood=function(par){
  -sum(dnorm(y,mean=par[1],sd=par[2],log=TRUE))
  }

#find the values that minimizes the function
#c(1,1) are the initial values for mu and sigma
fit <- optim(par=c(1,1), fn=neg.log.likelihood,
             method="L-BFGS-B",
             lower=c(0,0),upper=c(10,1)
             )

#Maximum likelihood estimates for mu and sigma
fit$par


## ----echo=TRUE--------------------------------------------------------------------------------------------------------------------------------------------
out = lm(y~1)
summary(out)

