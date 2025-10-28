
# Libraries
  library(ggplot2)
  library(viridis)

# Function for estimating p - the probability of a yes to question A
  p.hat = function(theta,n.yes,n){1/(2*theta-1)*(n.yes/n)-((1-theta)/(2*theta-1))}

# Probability of getting question A
  theta = 0.8

# Probability of getting question B
  theta.prime =1-theta
  
# Probabilty of a yes to question A
  p = 0.2

# Probabilty of a yes to question B
  p.prime = 1-p
  
  
# Probabilty of getting a yes
  p.yes = theta*p+theta.prime*p.prime

  
# Number of participants
  n1 = 50
  n2 = 250
  n3 = 500  
  
# Simulate the number of yes'
  nsim=1000
  y1 = rbinom(nsim, n1, p.yes)
  y2 = rbinom(nsim, n2, p.yes)
  y3 = rbinom(nsim, n3, p.yes)
  
# For each sample, estimate p
  
p.hat.values1 =  p.hat(theta,y1,n1)
p.hat.values2 =  p.hat(theta,y2,n2)
p.hat.values3 =  p.hat(theta,y3,n3)

hist(p.hat.values)  



plot.dat = data.frame(p.est = c(p.hat.values1,p.hat.values2,p.hat.values3),
                      n = c(rep("n1",nsim),rep("n2",nsim),rep("n3",nsim))
                      )

ggplot(plot.dat, aes(x=p.est, group=n, fill=n)) +
  geom_density(adjust=1.5, alpha=.4)

###################################################

# invesigate sampling standard deviation of p-hat by varying p and theta

p = seq(0.05,0.95,by=0.1)
theta = seq(0.05,0.95,by=0.1)
nsim=1000
n = 100

save.matrix=matrix(NA, nrow=length(p),ncol=length(theta))

for(i in 1:length(p)){
  p.use=p[i]
  for(j in 1:length(theta)){
    theta.use=theta[j]
    
    # Probabilty of getting a yes
    p.yes = theta.use*p.use+(1-theta.use)*(1-p.use)
    
    # Simulate the number of yes'
    y = rbinom(nsim, n, p.yes)
    
    # For each sample, estimate p
    p.hat.values =  p.hat(theta.use,y,n)
    
    save.matrix[i,j] = sd(p.hat.values)
    
    
  }#end theta loop
  
}#end p loop



# install.packages("plot.matrix")
library(plot.matrix)
par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(save.matrix,xlab="theta",ylab="p",col=viridis::viridis(100))
