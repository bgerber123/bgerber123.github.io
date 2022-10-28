library(purrr)
N=200
set.seed(4343)
age=rdunif(N, a=4, b=30)
table(age)
hist(age)

sex=rdunif(N, a=0, b=1)
table(sex)

sex.factor=sex
sex.factor[which(sex==0)]="Male"
sex.factor[which(sex==1)]="Female"

#Simulate effects of site
n.sites=5
beta.site=rnorm(n.sites,0,400)

#Simulate Site index
set.seed(35435)
site.index=rdunif(N, a=1, b=n.sites)
table(site.index)


site.index.name=site.index
site.index.name[which(site.index==1)]="Kenya.1"
site.index.name[which(site.index==2)]="Kenya.2"
site.index.name[which(site.index==3)]="Kenya.3"
site.index.name[which(site.index==4)]="Kenya.4"
site.index.name[which(site.index==5)]="Kenya.5"


site.coef=site.index
site.coef[which(site.index==0)]=beta.site[1]
site.coef[which(site.index==1)]=beta.site[2]
site.coef[which(site.index==2)]=beta.site[3]
site.coef[which(site.index==3)]=beta.site[4]
site.coef[which(site.index==4)]=beta.site[5]

site.coef

beta0= 13000-2000 #(males) lbs
beta1= (1300-6600)-4000 #(female differences) lbs
beta2=  50 # age
beta3=150

mu=beta0+beta1*sex+beta2*age+beta3*(age*sex)+site.coef

sigma=10

set.seed(434312)
y=rnorm(N, mean=mu,sd=sigma)

#these are observed weights
y

dat=data.frame(y,sex.factor,age,site.index.name)
colnames(dat)=c("weight","sex","age.years","site")

write.csv(dat, file="elephant.study.csv",row.names = FALSE)


dat=read.csv("elephant.study.csv")

a=glm(weight~site,contrasts = list(site = contr.sum),data=dat)

site=as.factor(dat$site)

model.matrix(~site,contrasts = list(site = contr.sum))

coef(a)[1]+sum(coef(a)[-1]*(-1))

c(511.5 - -487.1    -   -821.2    -    498.4)

rbind(unique(round(as.numeric(predict(a))),digits=0),
unique(dat$site))

glm(y~sex*age+site,data=dat)
