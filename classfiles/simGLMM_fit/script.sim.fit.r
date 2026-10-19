
# Objective: simulate data from a mixed-effects model


library(lme4)
library(glmmTMB)
source("sim.data.logistic.mixed.model.r")

# Define attributes of thr grouping variable
  n_groups =20
  n_per_group = 40
  
  beta_0 <- -1       # Fixed Intercept
  beta_1 <-  1       # Effect of continuous covariate X1
  
  sd_slope = 1


# single data set simulation
sim.data = sim.data.logistic.mixed.model(n_groups,
                                         n_per_group,
                                         beta_0,
                                         beta_1,
                                         sd_slope
                                         )


#Fit model 
 glmm_fit <- glmer(y ~ x + (0+x | group), 
                   data = sim.data, 
                   family = binomial(link = "logit"))
 
 glmm_fit <- glmmTMB(y ~ x + (0+x | group), 
                   data = sim.data, 
                   family = binomial(link = "logit"))
 

# Could also fit in brms
brm.fit = brm(formula = y ~ x + (0+x | group),  
              data = sim.data, 
              family = bernoulli(link = "logit"),
              warmup = 1000, 
              iter = 5000, 
              chains = 1, 
              cores = 1
)
##############################
# Simulate lots of datasets
n.sim=200
sim.data.reps = replicate(n.sim,
                          sim.data.logistic.mixed.model(n_groups,
                                                   n_per_group,
                                                   beta_0,
                                                   beta_1,
                                                   sd_slope
                          )
)


length(sim.data.reps)
dim(sim.data.reps)
head(sim.data.reps)

# in order, row1 = group, row2 = x, row3 = y, row 4 = parms
save.parms=matrix(NA, nrow=n.sim, ncol=3)
for( i in 1:n.sim){
  #i=1
  sim.data = data.frame(group = sim.data.reps[1,i]$group,
                         x = sim.data.reps[2,i]$x,
                         y = sim.data.reps[3,i]$y
                        )
  
  glmm_fit <- glmmTMB(y ~ x + (0+x | group), 
                    data = sim.data, 
                    family = binomial(link = "logit"))
  
  vc <- VarCorr(glmm_fit)
  
  save.parms[i,] = c(fixef(glmm_fit)[[1]],vc$cond$group[1])
  
  #save.parms[i,]=c(glmm_fit@beta,glmm_fit@theta)
print(i)  
}


hist(save.parms[,1],main="beta0")
abline(v=beta_0,lwd=3,col=1)

hist(save.parms[,2],main="beta1")
abline(v=beta_1,lwd=3,col=1)

hist(save.parms[,3],main="sd")
abline(v=sd_slope,lwd=3,col=1)
