## ----echo = TRUE---------------------------------------------------------------------------
  Group1.Mean <- 100
  Group1.SD <- 20
  
  Group2.Mean <- 120
  Group2.SD <- 20


## ----echo = FALSE, eval=TRUE,fig.align='center',fig.height=4-------------------------------

  par(cex.lab=1.5,cex.axis=1.5)
  curve(dnorm(x,Group1.Mean,Group1.SD ),xlim=c(0,200),lwd=5,ylab="Density",xlab="Hummingbird visits per day")
  curve(dnorm(x,Group2.Mean,Group2.SD ),xlim=c(0,200),lwd=5,add=TRUE,col=2)
  legend("topleft",legend=c("Feeder 1","Feeder 2"),col=c("black","red"),lwd=3,cex=1.5)



## ----echo = TRUE---------------------------------------------------------------------------
library(pwr)
# Wish to test a difference b/w groups 1 and 2
# Want to know if there is a difference in means
 
#Difference in Means
  effect.size <- Group1.Mean-Group2.Mean

#Group st. dev
  group.sd <- sqrt(mean(Group1.SD^2,Group2.SD^2))

#Mean difference divided by group stdev
#How does the numerator and denominator influence this number?  
  d <- effect.size/group.sd




## ----echo=TRUE-----------------------------------------------------------------------------
power = 0.8

out = pwr.t.test(d=d,power=power,type="two.sample",
                 alternative="two.sided")

#Sample Size Needed for each Group
out$n


## ----echo=TRUE-----------------------------------------------------------------------------
power = matrix(seq(0.8,0.99,by=0.01))

my.func = function(x){                    
                      pwr.t.test(d=d,power=x,
                                 type="two.sample",
                                 alternative="two.sided"
                                )$n
}

out= apply(power,1, FUN=my.func)



## ----echo=FALSE,fig.align='center', fig.height=3.5-----------------------------------------
#Sample Sizes Needed for each Group for different power levels
#Change to total sample size
out=out*2
par(cex.lab=1.5,cex.axis=1.5,mar=c(5,5,1,1))
plot(power,out,type="b",ylab="Sample Size Per Group",
     xlab="Power",lwd=5)


## ----echo=TRUE-----------------------------------------------------------------------------
  Group1.Mean <- 100
  Group1.SD <- 20
  
  Group2.Mean <- 120
  Group2.SD <- 20


## ----echo=TRUE-----------------------------------------------------------------------------
#setup combinations of alpha and power
  power = seq(0.8,0.99,by=0.01)
  alpha= seq(0.01,0.2,by=0.01)
  x=expand.grid(power,alpha)
  dim(x)
  colnames(x)=c("Power","alpha")
  head(x)



## ----echo=TRUE-----------------------------------------------------------------------------
#make new function and use mapply
  fun1=function(a,b){
              pwr.t.test(d=d,power=a,type="two.sample",
                         alternative="two.sided",sig.level=b)$n
                    }
  out= mapply(fun1,a=x$Power,b=x$alpha)
  length(out)

#Change per group sample size to total sample size  
  out=cbind(x,out*2)
  colnames(out)[3]="n"
  head(out)



## ----echo=FALSE, fig.height=8, fig.width=10------------------------------------------------
library(plotly)
fig <- plot_ly(out, x = ~Power, y = ~alpha, z = ~n, marker = list(size = 5))
fig <- fig %>% add_markers(color=~n)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Power'),
                     yaxis = list(title = 'Alpha'),
                     zaxis = list(title = 'Sample Size')))
#fig <- fig %>%  layout( xaxis = list(nticks=10, tickmode = "auto"))
fig


