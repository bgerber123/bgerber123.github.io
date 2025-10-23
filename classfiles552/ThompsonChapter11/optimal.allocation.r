
# Read in data
  dat=read.csv("N.fish.csv")
  head(dat)
  colnames(dat)=c("Index","Count")


# Look at distribution / groupings
  hist(dat$Count)
  plot(dat$Index,dat$Count)

# Define strata by the possible groupings
  index1=which(dat$Count<=50)
  index2=which(dat$Count>50 & dat$Count<=150)
  index3=which(dat$Count>150 & dat$Count<=450)
  index4=which(dat$Count>450)
 
# Create a new variable and assign names  
  dat$strata=dat$Count
  dat$strata[index1]="Strata1"
  dat$strata[index2]="Strata2"
  dat$strata[index3]="Strata3"
  dat$strata[index4]="Strata4"
  dat$strata

# Get variance and length (for N_h)
  strata.var=aggregate(dat$Count, by=list(dat$strata),FUN=var)$x
  strata.size=aggregate(dat$Count, by=list(dat$strata),FUN=length)$x

# sample size
  n = 100

# Eqn in Chapter 11, section 11.5
optimal.nh = n*strata.size*sqrt(strata.var) / sum(strata.size*sqrt(strata.var))

optimal.nh

#change n and see that the optimal proportional allocation doesn't change
  optimal.nh/n
