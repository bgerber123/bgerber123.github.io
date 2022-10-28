library(rstanarm)

y=c(0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0)

dat=data.frame(y=y)

#t_prior <- student_t(df = 7, location = 0, scale = 2.5)
post1 <- stan_glm(y~1, data = dat,
                 family = binomial(link = "logit"))

summary(post1)
post1$coefficients
plogis(post1$coefficients)

post1$fitted.values

##############################

y=read.csv("counts.trees1.csv")
dat=data.frame(y=y)
colnames(dat)="y"

#t_prior <- student_t(df = 7, location = 0, scale = 2.5)
post1 <- stan_glm(y~1, data = dat,
                 family = poisson(link = "log"))

plot(post1)
summary(post1)
post1$coefficients

post1$fitted.values
