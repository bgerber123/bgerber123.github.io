#Adapted code to similate three difffernet datasets


# ---------------------------------------------------------------
# Code for the book 'Applied Statistical Modeling for Ecologists' 
# by Kéry & Kellner, Elsevier, 2024
# ---------------------------------------------------------------

# ---------------------------------
# Chapter 20  --  Integrated models
# ---------------------------------

# Last changes: 12 June 2024


# 20.1 Introduction
# -----------------
# (no code)


# 20.2 Data generation: simulating three abundance data sets 
#      with different observation/aggregation models
# ----------------------------------------------------------

set.seed(20)

# Simulate the two data sets and plot them
# Choose sample size and parameter values for both data sets
nsites1 <- 500                                         # Sample size for count data
nsites2 <- 1000                                        # Sample size for zero-truncated counts
nsites3 <- 2000                                        # Sample size for detection/nondetection data
mean.lam <- 2                                          # Average expected abundance (lambda) per site
beta <- -2                                             # Coefficient of elevation covariate on lambda
truth <- c("log.lam" = log(mean.lam),"beta" = beta)    # Save truth

# Simulate elevation covariate for all three and standardize to mean of 1000 and
# standard deviation also of 1000 m
elev1 <- sort(runif(nsites1, 200, 2000))               # Imagine 200–2000 m a.s.l.
elev2 <- sort(runif(nsites2, 200, 2000))
elev3 <- sort(runif(nsites3, 200, 2000))
selev1 <- (elev1-1000)/1000                            # Scaled elev1
selev2 <- (elev2-1000)/1000                            # Scaled elev2
selev3 <- (elev3-1000)/1000                            # Scaled elev3

# Create three regular count data sets with log-linear effects
C1 <- rpois(nsites1, exp(log(mean.lam) + beta * selev1))
C2 <- rpois(nsites2, exp(log(mean.lam) + beta * selev2))
C3 <- rpois(nsites3, exp(log(mean.lam) + beta * selev3))

table(C1)                                              # Tabulate data set 1

# Create data set 2 (C2) by zero-truncating (discard all zeroes)
ztC2 <- C2                                             # Make a copy
ztC2 <- ztC2[ztC2 > 0]                                 # Tossing out zeroes yields zero-truncated data

table(C2); table(ztC2)                                 # tabulate both original and ZT data set

# Turn count data set 3 (C3) into detection/nondetection data (y)
y <- C3                                                # Make a copy
y[y > 1] <- 1                                          # Squash to binary

table(C3) ; table(y)                                   # tabulate both original counts and DND


data=list(Counts=data.frame(Counts=C1,elev1=elev1,selev1=selev1),
                        ZTCounts=data.frame(ZTCounts=ztC2,elev2=elev2[C2 > 0],selev2=selev2[C2 > 0]), 
                        Detection = data.frame(y=y,elev3=elev3,selev3=selev3)
)

data[[1]]

save(data,file="three.data.sets")
