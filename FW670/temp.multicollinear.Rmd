## Multi-Collinearity

![](/img/graham.png){fig-align="center" width="275"}

. . .

Correlation among explantory variables

-   Can happen when $r \geq 0.28$ or $r^2 \geq 0.08$
-   causes inaccurate estimation
-   decreases statistical power
-   leads to exclusion of important predictor variables


## Multi-Collinearity (Code) {.scrollable}

```{r,eval=TRUE, echo=TRUE}
library(faux)
n=100
set.seed(543531)
x.var <- rnorm_multi(n = n, 
                  mu = c(10, 20),
                  sd = c(1, 1),
                  r = c(0), 
                  varnames = c("A", "B"),
                  empirical = FALSE)

set.seed(54353)
x.var.cor <- rnorm_multi(n = n, 
                  mu = c(10, 20),
                  sd = c(1, 1),
                  r = c(0.8), 
                  varnames = c("A", "B"),
                  empirical = FALSE)

#Correlation
  cor(x.var)
  cor(x.var.cor)

```

## Multi-Collinearity (Code)
```{r,eval=TRUE, echo=TRUE, fig.align='center'}
par(mfrow=c(1,2))
  plot(x.var.cor$A,x.var.cor$B)
  plot(x.var$A,x.var$B)
```

## Multi-Collinearity (Code)

```{r,eval=TRUE, echo=TRUE}

#Design matrices
  X.cor=model.matrix(~x.var.cor$A+x.var.cor$B)
  X=model.matrix(~x.var$A+x.var$B)

#True coefs  
  beta=c(1,2,3)

#Derive mu  
  mu=X%*%beta
  mu.cor=X.cor%*%beta

#simulate data  
  set.seed(54353)
  y=rnorm(n,mu,2)
  y.cor=rnorm(n,mu.cor,2)
```

## Model Estimates
```{r,eval=TRUE, echo=TRUE}
summary(glm(y~0+X))
```


## Model Estimates
```{r,eval=TRUE, echo=TRUE}
summary(glm(y~0+X.cor))
```