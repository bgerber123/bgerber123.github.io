# Connection b/w mean count and probability of occurence


# Go to Page 478 / Chapter 20 / Integrated Models

# psi = probability of occurrence
# lamdba = mean count 

# psi is the P(y=1)
# which is equal to P(N>0)
# which is equal to 1-P(N=0)
# which is equal to 1-exp(-lambda)


#Probability of observing > 0 counts, given lambda = 2 
ppois(0,lambda=2,lower.tail = FALSE)

#also...
1-exp(-2)

#also...
1- ppois(0,lambda=2,lower.tail = TRUE)



#########################

lambda=c(0,0.1,0.5,1,10,20)
psi = 1 - exp(-lambda)
psi


#If we believe that then lets re-arrange
all.equal(
  exp(-lambda) ,
  1 - psi,
  tolerance = 0.00000001
)


all.equal(
  log(exp(-lambda)),
  log(1-psi),
  tolerance = 0.00000001
)

all.equal(
  -lambda,
  log(1-psi),
  tolerance = 0.00000001,
)  


all.equal(
  lambda,
  -log(1-psi),
  tolerance = 0.00000001
)

all.equal(
  log(lambda),
  log(-log(1-psi)),
  tolerance = 0.00000001
)

# This part log(-log(1-psi)), is called the complimentary log-log link
# We can then model lambda on natural scale from binary data using this log-log link function

