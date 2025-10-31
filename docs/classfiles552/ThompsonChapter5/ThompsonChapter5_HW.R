###########################
# Chapter 5 Problem 1

n = 1200
N = 1800000
y.sum = 552
p.hat= 552/1200
p.var= (N-n)/N * (p.hat*(1-p.hat))/(n-1)

p.lcl = p.hat + qt(0.025,n-1)*sqrt(p.var)
p.ucl = p.hat + qt(0.975,n-1)*sqrt(p.var)


p.hat
p.lcl
p.ucl


###########################
# Chapter 5 Problem 3

N = 1500
d = 0.02
p = 0.5 #worse case scenario
alpha = 0.05
z = qnorm(1-0.05/2)

n = (N*p*(1-p)) / ((N-1)*(d^2/z^2)+p*(1-p))

#  923 sample units will suffice
n
