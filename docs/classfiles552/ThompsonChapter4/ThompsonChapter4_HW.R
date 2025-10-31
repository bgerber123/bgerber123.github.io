###########################
# Chapter 4 Problem 1

# To within 500 trees

N = 1000
sigma2 = 45
d = 500
alpha = 0.05
z = qnorm(1-0.05/2)
n = 1/(d^2/(N^2*z^2*sigma2)+(1/N))

# 408.79
n


# To within 1000 trees

d = 1000
n = 1/(d^2/(N^2*z^2*sigma2)+(1/N))

# 147.38
n


# To within 2000 trees

d = 2000
n = 1/(d^2/(N^2*z^2*sigma2)+(1/N))

# 41.42
n

###########################

#Problem from class

# Solve for d when
# N = number of areas in a lake with bass = 200
# Abs. difference (d) is 10
# 95% confidence, $\alpha = 0.05$
# sigma could range from 10 to 200

N = 200
n = 10
z = qnorm(1-alpha/2)
sigma = seq(10,100,by=1)
sigma2=sigma^2

inputs= expand.grid(n,sigma2,z,N)
colnames(inputs)=c("n","sigma2","z","N")
inputs$z = z
inputs$N = N

# Create function to solve for d

# d = sqrt((z^2*sigma2)/n - (z^2*sigma2)/N)
# d = z*sqrt((N-n)/N * (sigma^2/n))
# same equation, the 2nd one is direct from the book

d.fn = function(inputs){
  sqrt( ((inputs[3]^2*inputs[2])/inputs[1]) - ((inputs[3]^2*inputs[2])/inputs[4]))
  
}

d = apply(inputs,1,d.fn)
inputs$d=d

head(inputs)

#worse case for d is 42.05 or 42
max(inputs$d)
which.max(inputs$d)

inputs[91:nrow(inputs),]

plot(sigma,inputs$d)




# test

sqrt((1.96^2*10)/12 - (1.96^2*10)/100)

1.96*sqrt((100-12)/100 * (10/12))

1.96*sqrt(10)*sqrt((1/12)-(1/100))


#
sqrt((1.96^2*100^2)/10 - (1.96^2*100^2)/200)

1.96*100*sqrt((1/10)-(1/200))

z * sqrt(((N-n)/N) * sigma2/n)
