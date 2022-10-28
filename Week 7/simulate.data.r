
n=10
n2=100
lambda=0.5

set.seed(14353)
y=rpois(n,lambda)
y2=rpois(n2,lambda)

y
y2

write.csv(y,file="counts.trees1.csv",row.names = FALSE)
write.csv(y2,file="counts.trees2.csv",row.names = FALSE)
