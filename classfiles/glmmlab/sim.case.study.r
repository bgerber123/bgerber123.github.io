
library(glmmTMB)

n.groups=5
n.per.group = 50

mu.beta = 0
sd.beta = 1

set.seed(145)
beta = rnorm(n.groups,mu.beta,sd.beta)
beta
beta0 = -2

x=NULL
for(i in 1:n.groups){
  set.seed(432423+i)
  x.dist = seq(100,5000,length.out=n.per.group)+rnorm(n.per.group,0,100)
  x=c(x,x.dist)
}

#x.sc = scale(x,center = TRUE,scale = TRUE)
x.sc = x/1000
x.sc
range(x.sc)

PA = rep(c("PA1","PA2","PA3","PA4","PA5"),each=n.per.group)

dat2=data.frame(PA=PA,x.sc=x.sc,x=x)

dat.list=split.data.frame(dat2,dat2$PA)

x.sc.within=unlist(lapply(dat.list,FUN=function(x){scale(x$x)}))

y=NULL
for(i in 1:n.groups){
  set.seed(432423+i)

  y = c(y,
        rbinom(n.per.group,1,plogis(beta0+beta[i]*dat.list[[i]]$x.sc))
  )
}


dat2 = data.frame(dat2,y=y,x.sc.within=x.sc.within)
head(dat2)

dat2$y

model = glmmTMB(y~x.sc+(0+x.sc||PA),data=dat2,family=binomial(link="logit"))

summary(model)
ranef(model)

marginaleffects::plot_predictions(model,
                                  condition=c("x.sc","PA"),
                                  type="response",
                                  re.form=NULL,
                                  vcov=TRUE
)


dat2$camera=rep(1:n.per.group,n.groups)

head(dat2)


model2 = glmmTMB(y~x.sc,data=dat2,family=binomial(link="logit"))

marginaleffects::plot_predictions(model2,
                                  condition=c("x.sc"),
                                  type="response",
                                  re.form=NULL,
                                  vcov=TRUE
)


model3 = glmmTMB(y~x.sc*PA,data=dat2,family=binomial(link="logit"))

marginaleffects::plot_predictions(model3,
                                  condition=c("x.sc","PA"),
                                  type="response",
                                  re.form=NA,
                                  vcov=TRUE
)


occ.out=data.frame(PA=dat2$PA,
                    camera=dat2$camera,
                    occur=dat2$y,
                    dist.human = dat2$x
)

write.csv(occ.out, file="rabbit.occ.data.csv")
