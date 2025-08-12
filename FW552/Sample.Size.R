## ----echo=FALSE,warning=FALSE,results='hide'----------------------------------
library(tidyverse)
library(kableExtra)
library(magrittr)
library(knitr)


## ----echo= TRUE---------------------------------------------------------------
  N = 200
  d = 10
  alpha  = 0.01
  z = qnorm(1-alpha/2)
  sigma2 = 20^2

  n = 1/(d^2/(z^2*sigma2)+(1/N))
  n



## ----echo = TRUE--------------------------------------------------------------
  Group1.Mean <- 100
  Group1.SD <- 20
  
  Group2.Mean <- 120
  Group2.SD <- 20


## ----echo = FALSE, eval=TRUE,fig.align='center',fig.height=4------------------
  par(cex.lab=1.5,cex.axis=1.5)
  curve(dnorm(x,Group1.Mean,Group1.SD ),xlim=c(0,200),lwd=5,ylab="Density",xlab="Hummingbird visits per day")
  curve(dnorm(x,Group2.Mean,Group2.SD ),xlim=c(0,200),lwd=5,add=TRUE,col=2)
  legend("topleft",legend=c("Feeder 1","Feeder 2"),col=c("black","red"),lwd=3,cex=1.5)


## -----------------------------------------------------------------------------
#| echo: TRUE
#| eval: TRUE
#| code-line-numbers: 1|2,3|5,6|8,9|11,12,13

library(pwr)
# Wish to test a difference b/w groups 1 and 2
# Want to know if there is a difference in means
 
#Difference in Means
  effect.size <- Group1.Mean-Group2.Mean

#Group st. dev
  group.sd <- sqrt(mean(c(Group1.SD^2,Group2.SD^2)))

#Mean difference divided by group stdev
#How does the numerator and denominator influence this number?  
  d <- effect.size/group.sd




## -----------------------------------------------------------------------------
#| echo: TRUE
#| eval: TRUE
#| code-line-numbers: 1|3,4|6,7

power = 0.8

out = pwr.t.test(d=d,power=power,type="two.sample",
                 alternative="two.sided")

#Sample Size Needed for each Group
out$n


## -----------------------------------------------------------------------------
#| echo: TRUE
#| eval: TRUE
#| code-line-numbers: 1|3,4,5,6,7,8|10

power = matrix(seq(0.8,0.99,by=0.01))

my.func = function(x){                    
                      pwr.t.test(d=d,power=x,
                                 type="two.sample",
                                 alternative="two.sided"
                                )$n
}

out= apply(power,1, FUN=my.func)



## ----echo=FALSE,fig.align='center', fig.height=3.5----------------------------
#Sample Sizes Needed for each Group for different power levels
#Change to total sample size
#out=out*2
par(cex.lab=1.5,cex.axis=1.5,mar=c(5,5,1,1))
plot(power,out,type="b",ylab="Sample Size Per Group",
     xlab="Power",lwd=5)
abline(h=c(20,25,30,35),col="grey")


## ----echo=TRUE----------------------------------------------------------------
  Group1.Mean <- 100
  Group1.SD <- 20
  
  Group2.Mean <- 120
  Group2.SD <- 20


## -----------------------------------------------------------------------------
#| echo: TRUE
#| eval: TRUE

# Allow group 1 to vary
  Group1.Mean <- seq(10,110,by=5)

#THIS IS THE SAME
  Group1.SD <- 20
  Group2.Mean <- 120
  Group2.SD <- 20
  group.sd <- sqrt(mean(Group1.SD^2,Group2.SD^2))

# Variable effect.size  
  effect.size <- Group1.Mean-Group2.Mean
  d <- effect.size/group.sd

#setup combinations of d and power
  power = seq(0.8,0.99,by=0.01)
  power.d = expand.grid(power,d)
  power.d$Var1 = as.numeric(power.d$Var1)
  


## -----------------------------------------------------------------------------
#| echo: TRUE
#| eval: TRUE

#make new function and use mapply
my.func = function(x,x2){                    
                      pwr.t.test(d=x2,power=x,
                                 type="two.sample",
                                 alternative="two.sided"
                                )$n
}

# mapply function
  out= mapply(power.d$Var1,power.d$Var2, FUN=my.func)

# unstandardized the effect size back to difference of means
  power.d$Var2=power.d$Var2*group.sd
  
  out2=cbind(power.d,out)
  colnames(out2)=c("power","d","n")



## ----echo=FALSE, fig.height=6, fig.width=10-----------------------------------
library(plotly)
fig <- plot_ly(out2, x = ~power, y = ~d, z = ~n, marker = list(size = 5))
fig <- fig %>% add_markers(color=~n)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Power'),
                     yaxis = list(title = 'd'),
                     zaxis = list(title = 'Sample Size')))
#fig <- fig %>%  layout( xaxis = list(nticks=10, tickmode = "auto"))
fig



## ----echo=FALSE, results='hide'-----------------------------------------------
#Output code into R file
knitr::purl(input="../FW552/Sample.Size.qmd",output="../FW552/Sample.Size.R")

