## Categorical Variable w/ 2 levels {.scrollable}
$$
  y_{i} \sim \text{Normal}(\mu_{i}, \sigma^2)\\
\mu_{i} = \beta_0+\beta_1\times x_i
$$
  where $x_i$ is either a zero or one, indicating whether the $i^{th}$ row is from <span style="color:blue">site 1</span> (*0*) or <span style="color:blue">site 2</span> (*1*).
  
  <br>
    
    This is called *Dummy Coding*.
  
  
  ## Categorical Variable w/ 2 levels {.scrollable}
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  # Setup data
  x=as.factor(rep(c("Site 1","Site 2"),n/2))
  levels(x)
  ```
  
  <br>
    
    . . .
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  # Turn the factor into 0 and 1's
  head(model.matrix(~x))
  
  x.var=model.matrix(~x)[,2]
  ```
  
  . . .
  
  <br>
    
    ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  # Parameters  
  b0=50
  b1=-20
  mu=b0+b1*x.var
  
  # Sample Data
  set.seed(43243)
  y=rnorm(n,mean=mu,sd=4)
  
  ```
  
  <br>
    
    . . .
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #fit the model 
  model2=glm(y~x)
  model2.1=glm(y~x.var)
  
  #comparison  
  rbind(coef(model2), coef(model2.1))
  ```
  
  ## Side-Bar: Maximum Likelihood Optimization {.scrollable}
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  
  #Here is our negative log-likelihood function with three
  #parameters - the mean (2) and stdev (1)
  neg.log.like = function(par) {
    mu=par[1]+par[2]*x.var
    sum(-dnorm(y,mean = mu,sd = par[3],log = TRUE))
  }
  ```
  
  <br>
    
    . . .
  
  ### Use our function in an optimization function
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #use optim with initial values and define the lower and upper limits of the possible values
  fit1 <- optim(
    par = c(0, 0,1),
    fn = neg.log.like,
    method = "L-BFGS-B",
    lower = c(-100, -100, 0.01),
    upper = c(400, 400, 100)
  )
  
  fit1$par
  ```
  
  <br>
    
    ## Categorical Variable w/ 2 levels {.scrollable}
    ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  summary(model2)
  ```
  
  ## Relevel {.scrollable}
  
  We can manipulate the factor levels of $x$ to indicate that <span style="color:blue">site 1</span> is denoted by a **1** now and <span style="color:blue">site 2</span> is denoted by a **0**.
  
  . . .
  
  <br>
    
    ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #change intercept meaning
  x.relev=relevel(x,ref="Site 2")
  levels(x.relev)
  ```
  
  . . .
  
  <br>
    
    ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #fit the model again
  model2.2=glm(y~x.relev)
  
  #Look at coefs  
  rbind(coef(model2),coef(model2.1),coef(model2.2))
  ```
  
  . . .
  
  <br>
    
    ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #compare predictions  
  rbind(predict(model2)[1:2],  
        predict(model2.1)[1:2],
        predict(model2.2)[1:2]  
  )
  ```
  
  ## Categorical Variable w/ 4 levels 
  ### Dummy Coding
  $$
    y_{i} \sim \text{Normal}(\mu_{i}, \sigma^2)\\
  \mu_{i} = \beta_0+(\beta_1\times x_{1,i}) + (\beta_2\times x_{2,i}) + (\beta_3\times x_{3,i})
  $$
    $x_{1,i} =$ indicator of <span style="color:blue">site 2</span> (1) or not (0)
  
  $x_{2,i} =$ indicator of <span style="color:blue">site 3</span> (1) or not (0)
  
  $x_{3,i} =$ indicator of <span style="color:blue">site 4</span> (1) or not (0)
  
  
  
  ## Categorical Variable w/ 4 levels {.scrollable}
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #Setup Data
  x=as.factor(rep(c("Site 1","Site 2","Site 3", "Site 4"),n/4))
  levels(x)
  
  #Convert factors to 0 and 1's
  head(model.matrix(~x))
  
  x.var=model.matrix(~x)[,2:4]
  ```
  
  <br>
    
    . . .
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  # Set Parameters  
  b0=50 #Site 1
  b1=-20 #Diff of site 2 to site 1
  b2=-200 #Diff of site 3 to site 1
  b3=100 #Diff of site 4 to site 1
  
  # Mean  
  mu = b0+b1*x.var[,1]+b2*x.var[,2]+b3*x.var[,3]
  
  #True mean group-level values
  unique(mu)
  
  #Grand Mean
  mean(unique(mu))
  ```
  
  <br>
    
    . . .
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  # Simulate Data  
  set.seed(43243)
  y=rnorm(n,mean=mu,sd=4)
  ```
  
  <br>
    
    . . .
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  # fit the model
  model3=glm(y~x)
  model3.1=glm(y~x.var)
  
  # Compare coefs    
  rbind(coef(model3), coef(model3.1))
  ```
  
  ## Effect Coding w/ 4 levels {.scrollable}
  
  [Effect Coding Link](https://stats.stackexchange.com/questions/52132/how-to-do-regression-with-effect-coding-instead-of-dummy-coding-in-r)
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  
  #Use effect coding to make the intercept the grand mean
  model3.2=glm(y~x,contrasts = list(x = contr.sum))
  
  
  # Intercept = grand mean of group-means
  # Coef 1 = effect difference of Site 1 from Grand Mean
  # Coef 2 = effect difference of Site 2 from Grand Mean
  # Coef 3 = effect difference of Site 3 from Grand Mean
  
  coef(model3.2)
  
  ```  
  
  <br>
    
    . . .
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #The coefficient for site-level 4 (difference from the grand mean)
  sum(coef(model3.2)[-1]*(-1))
  ```
  
  <br>
    
    . . .
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #Predict the values and compare them to the true means for
  #each site
  rbind(unique(mu),
        predict(model3.2)[1:4])
  ```
  
  ## Continuous Variable
  
  ADD here and mean-centering
  ```{r,eval=TRUE,echo=TRUE}
  
  #lm(Temperature ~ I(Chirps - mean(Chirps)), data = CricketChirps)
  
  ```
  
  
  ## Additive Model
  
  #### Categorical (2 levels) and Continuous Variable
  
  $$
    y_{i} \sim \text{Normal}(\mu_{i}, \sigma^2)\\
  \mu_{i} = \beta_0+(\beta_1\times x_{1,i}) + (\beta_2\times x_{2,i})
  $$
    $x_{1,i} =$ indicator of <span style="color:blue">site 2</span> (1) or not (0)
  
  $x_{2,i} =$ is a continuous numeric value
  
  
  ## Additive Model {.scrollable}
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #A continuous and categorical variable 
  x=as.factor(rep(c("Site 1","Site 2"),n/2))
  levels(x)
  x.var=model.matrix(~x)[,2]
  ```
  
  <br>
    
    . . .
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #Simulate x2 variable
  set.seed(54334)
  x2=rpois(n,100)
  
  #Parameters
  b0=50
  b1=-50
  b2=4
  
  #Mean  
  mu=b0+b1*x.var+b2*x2
  
  #Simualte Date  
  set.seed(43243)
  y=rnorm(n,mean=mu,sd=50)
  
  # fit the model
  model4=glm(y~x+x2)
  
  coef(model4)
  
  ```
  
  <br>
    
    . . .
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  #Confidence intervals of coefs
  confint(model4)
  
  ```
  
  . . .
  
  <br>
    
    ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  # Summary  
  summary(model4)
  ```
  
  . . .
  
  <br>
    
    ```{r, echo=TRUE, include=TRUE, eval=TRUE}
  
  # Fitted Values
  newdata=expand.grid(x,x2)
  head(newdata)
  colnames(newdata)=c("x","x2")
  
  
  preds=predict(model4,newdata=newdata,type="response",
                se.fit = TRUE)
  ```
  
  ## Additive Model Plot 1
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}  
  library(sjPlot)
  plot_model(model4, type = "pred", terms = c("x"))
  ```
  
  ## Additive Model Plot 2
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}  
  plot_model(model4, type = "pred", terms = c("x2","x"))
  ```
  
  ## Interaction Model
  
  #### Categorical (2 levels) and Continuous Variable
  
  $$
    y_{i} \sim \text{Normal}(\mu_{i}, \sigma^2)\\
  \mu_{i} = \beta_0+(\beta_1\times x_{1,i}) + (\beta_2\times x_{2,i}) + (\beta_3*(x_{1,i}\times x_{2,i}))
  $$
    $x_{1,i} =$ indicator of <span style="color:blue">site 2</span> (1) or not (0)
  
  $x_{2,i} =$ is a numeric value
  
  $x_{1,i} \times x_{2,i}=$ is zero for <span style="color:blue">site 1</span> values and the numeric value for site 2 values
  
  
  
  ## Interaction Model {.scrollable}
  
  ### Categorical (2 levels) and Continuous Variable
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}  
  
  # Simulate Variables
  x=as.factor(rep(c("Site 1","Site 2"),n/2))
  levels(x)
  x.var=model.matrix(~x)[,2]
  
  set.seed(5453)
  x2=rpois(n,100)
  
  # Parameters 
  b0=50
  b1=-50
  b2=4
  b3=-20
  
  # Mean  
  mu = b0+b1*x.var+b2*x2+b3*(x.var*x2)
  
  #Simulate Data
  set.seed(43243)
  y=rnorm(n,mean=mu,sd=10)
  
  # fit the model
  model5=glm(y~x2*x)
  model5.1=glm(y~x+x2+x:x2)
  
  #comparison  
  rbind(coef(model5),coef(model5.1))
  ```
  
  . . .
  
  <br>
    
    ```{r, echo=TRUE, include=TRUE, eval=TRUE}  
  #Confidence intervals of coefs
  confint(model5)
  ```
  
  ## Interaction Model
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}  
  #Summary  
  summary(model5)
  ```
  
  ## Interaction Model Plot
  
  ```{r, echo=TRUE, include=TRUE, eval=TRUE}  
  theme_set(theme_sjplot())
  plot_model(model5, type = "pred", terms = c("x2","x"))
  ```
  