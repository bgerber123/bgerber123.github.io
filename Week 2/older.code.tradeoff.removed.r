Is there a tradeoff b/w Type I ($\alpha$) and Type II error ($\beta$)?

. . .

<br>

How would we evaluate this?

. . .


```{r,echo=TRUE}
#| echo: TRUE
#| eval: TRUE

#setup combinations of alpha and power
  power = seq(0.8,0.99,by=0.01)
  alpha= seq(0.01,0.2,by=0.01)
  x=expand.grid(power,alpha)
  dim(x)
  colnames(x)=c("Power","alpha")
  head(x)

```  


. . .

```{r,echo=TRUE}
#| echo: TRUE
#| eval: TRUE
  
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

```



## Tradeoffs (Power, Type I Error, N)

```{r,echo=FALSE, fig.height=8, fig.width=10}
library(plotly)
fig <- plot_ly(out, x = ~Power, y = ~alpha, z = ~n, marker = list(size = 5))
fig <- fig %>% add_markers(color=~n)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Power'),
                     yaxis = list(title = 'Alpha'),
                     zaxis = list(title = 'Sample Size')))
#fig <- fig %>%  layout( xaxis = list(nticks=10, tickmode = "auto"))
fig

```
