# African Elephant Data
rdu<-function(n,lower,upper) sample(lower:upper,n,replace=T)

N=50
set.seed(4343)
age=rdu(N, lower=10, upper=50)
table(age)
hist(age)

latitude = runif(n,-30,10)


beta0 = 13000 #(males) lbs
beta1 =  0 # age
beta2 = -150

mu=beta0+beta1*age+beta2*latitude

sigma=20

set.seed(134312)
y=rnorm(N, mean=mu,sd=sigma)


#these are observed weights
y

dat=data.frame(y,age,latitude)
colnames(dat)=c("weight","age.years","lat")

write.csv(dat, file="elephant.study.csv",row.names = FALSE)
