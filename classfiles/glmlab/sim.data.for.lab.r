#Simulate occurence data for lynx

n = 50
set.seed(43243)
dist.road = runif(n, 20, 2000)
cover = runif(n, 0, 1)

dist.road.sc=scale(dist.road)
cover.sc=scale(cover)

X = model.matrix(~dist.road.sc+cover.sc)

beta = c(0, 2, 1)

lt = X%*%beta
p = plogis(lt)

set.seed(5435)
y =rbinom(n, 1,prob=p)

y

model = glm(y~dist.road.sc+cover.sc, family = binomial(link="logit"))
summary(model)
marginaleffects::plot_predictions(model,condition=c("dist.road.sc","cover.sc"))


###################
#simulation
n.sim=1000

n = 50
dist.road2 = rnorm(n)
cover2 = rnorm(n)

X2 = model.matrix(~dist.road2+cover2)

beta2 = c(-0.08, 2.5, 0.29)

lt2 = X2%*%beta2
p2 = plogis(lt2)

y =replicate(n.sim,rbinom(n, 1,prob=p))
dim(y)

fit = apply(y,2,FUN=function(x){
  m = glm(x~dist.road2+cover2)
  c(summary(m)$coef[2,4],
  summary(m)$coef[3,4])
  
})

dim(fit)

hist(fit[1,])
hist(fit[2,])

length(which(fit[1,]<0.05))/ncol(fit)

length(which(fit[2,]<0.05))/ncol(fit)
