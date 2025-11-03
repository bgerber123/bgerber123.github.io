
n=50
theta = 0.3
p = 0.1

prob.yes = theta*p+(1-theta)*(1-p)

prob.yes

n.sim=10000
y.save = rbinom(n=n.sim,size=n,prob=prob.yes)

hist(y.save)

rand.survey.est=function(theta,y,n){
  1/(2*theta-1)*(y/n)-((1-theta)/(2*theta-1))
  }

p.samp.dist=rand.survey.est(theta,y.save,n)

sd(p.samp.dist)

hist(p.samp.dist)
abline(v=p,lwd=3,col=2)
