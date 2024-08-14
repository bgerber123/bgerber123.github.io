#Simulate occurence data for lynx

n = 75
set.seed(43243)
dist.road = runif(n, 20, 2000)
cover = runif(n, 0, 1)

dist.road.sc=scale(dist.road)
cover.sc=scale(cover)

X = model.matrix(~dist.road.sc*cover.sc)

beta = c(0, 2, 1.5,-3)

lt = X%*%beta
p = plogis(lt)

set.seed(5435)
y =rbinom(n, 1,prob=p)

y

model = glm(y~dist.road.sc*cover.sc, family = binomial(link="logit"))
summary(model)
marginaleffects::plot_predictions(model,condition=c("dist.road.sc","cover.sc"))


lynx.data = data.frame(y=y, 
                       dist.road = dist.road,
                       cover = cover)
write.csv(lynx.data, file="lynx.data.csv")

###################
#simulation
n.sim=2000

n = 75
dist.road2 = rnorm(n)
cover2 = rnorm(n)

X2 = model.matrix(~dist.road2*cover2)

beta2 = c(0.5149, 1.6223, 0.6938, -2.5295)

lt2 = X2%*%beta2
p2 = plogis(lt2)


y =replicate(n.sim,rbinom(n, 1, prob = p2))
dim(y)

fit = apply(y,2,FUN=function(x){
  m = brglm::brglm(x~dist.road2*cover2, family = binomial(link="logit"))
  
  m
  c(summary(m)$coef[2,4],
  summary(m)$coef[3,4],
  summary(m)$coef[4,4],
  model$converged)

  
})

dim(fit)

hist(fit[1,])
hist(fit[2,])
hist(fit[3,])

which(fit[4,]==0)

length(which(fit[1,]<0.05))/ncol(fit)

length(which(fit[2,]<0.05))/ncol(fit)

length(which(fit[3,]<0.05))/ncol(fit)
