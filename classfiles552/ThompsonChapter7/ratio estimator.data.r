# Create some data

set.seed(54334)
cover = sample(seq(0,100,by=0.01),100)

N=length(cover)

beta=1.3
error=rnorm(N,0,10)
y = beta*cover+error

#y are counts of beetles.
#cover is ground cover

y=data.frame(y=y,cover=cover)

#Sample the y's

n = 10
set.seed(43543)
sample.index = sample(1:N,n)

y.sample = y[sample.index,]


plot(y.sample$cover,y.sample$y)

#population mean of x's
mu.cover = mean(y$cover)

#true beetle mean
mu.beetle = mean(y$y)

# Calculate ratio estimator and compare to sample mean

mean.sample = mean(y.sample$y)
mean.sample

mean.ratio = mean(y.sample$y)/mean(y.sample$cover) * mu.cover
mean.ratio

r = mean(y.sample$y)/mean(y.sample$cover)
var.mean.ratio = 1/(n-1)*sum((y.sample$y-r*y.sample$cover)^2)

pop.var.mean.ratio = var.mean.ratio/n * (1/(n-1))
pop.var.mean.ratio

#output data

index.excluded=(1:N)[-sample.index]

y.out = y
y.out$y[index.excluded]=NA

colnames(y.out) = c("beetle.count","cover")

write.csv(y.out,file="BeetleCover.Study1.csv")

##################################
# Second data


set.seed(54334)
cover = sample(seq(0,100,by=0.01),100)

N=length(cover)

beta=0.01
error=rnorm(N,0,10)
y = 50+beta*cover+error

#y are counts of beetles.
#cover is ground cover

y=data.frame(y=y,cover=cover)

#Sample the y's

n = 10
set.seed(4354)
sample.index = sample(1:N,n)

y.sample = y[sample.index,]


plot(y.sample$cover,y.sample$y)
cor(y.sample$cover,y.sample$y)

#population mean of x's
mu.cover = mean(y$cover)

#true beetle mean
mu.beetle = mean(y$y)

# Calculate ratio estimator and compare to sample mean

mean.sample = mean(y.sample$y)
mean.sample

mean.ratio = mean(y.sample$y)/mean(y.sample$cover) * mu.cover
mean.ratio

r = mean(y.sample$y)/mean(y.sample$cover)
var.mean.ratio = 1/(n-1)*sum((y.sample$y-r*y.sample$cover)^2)

pop.var.mean.ratio = var.mean.ratio/n * (1/(n-1))
pop.var.mean.ratio


#output data

index.excluded=(1:N)[-sample.index]

y.out = y
y.out$y[index.excluded]=NA

colnames(y.out) = c("beetle.count","cover")

write.csv(y.out,file="BeetleCover.Study2.csv")


