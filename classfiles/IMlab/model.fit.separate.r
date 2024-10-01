# Model fitting

load("three.data.sets")

#List elements

# Counts with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
head(data[[1]])

# Zero truncated Counts with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
head(data[[2]])

# Detection non-detection data with elevation covariate and scaled elevation covariate at a mean of 1000 and 
# standard deviation of 1000
head(data[[3]])


#######################
#######################
# Likelihood

# First, lets fit our Count data
fm1 <- glm(Counts ~ selev1, family = poisson(link = "log"),data=data[[1]])

summary(fm1)
exp(coef(fm1)[1])        


#Second, fit Zero truncated data
library(VGAM)
fm2 <- vglm(ZTCounts ~ selev2, family = pospoisson(), data = data[[2]])
summary(fm2)

exp(coef(fm2)[1])   
#Third, fit detection non-detection data
fm3 <- glm(y ~ selev3, family = binomial(link = "cloglog"), data = data[[3]])

summary(fm3)
exp(coef(fm3)[1])   



#######################
#######################
# Bayesian

