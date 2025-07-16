
##############
#  MODULE 2  #
##############

# Chapter 2: Simple Random Sampling
# 2.8. Computing Notes

  # Entering Data in R

  # To set the variable x to one specific value:
  x <- 20
  
  # To set x to a set of values, say, 3, 7, 5, and 2, 
  # use the combine function “c”:
  x <- c(3,7,5,2)
  
  # This combines the five values into a vector named “x”.

  # Another way is to type “scan()” and at the prompt 
  scan()
  # enter the values with spaces between them, rather than commas. 
  # Hit enter twice when you are done.

  # Check that x is what you want by typing either “x” or “print(x)”. 
  x
  print(x)

  # To read data into R from a spreadsheet program such as Excel, 
  # the easiest way is to save the data while in the spreadsheet 
  # program as a text file with values separated by commas. 
  # Then the file can be read into R with the read.table
  # command or the more specialized read.csv command. 
  # The details will be provided later whenever we need one of these functions.
  # Here are some simple R commands to get started:

  # everything after "#" is a comment line. You don't need to
  # type these lines.
  # type the following commands into the R command window.
  # produce the x and y coordinates for a randomly
  # distributed population of objects in space (eg, trees
  # or animals):
  set.seed(432432) #This forces the random draws to be same everytime you run the code
  popnx <- runif(100)
  popny <- runif(100)

  # plot the spatial distribution of the population
  plot(popnx,popny)

  # change the size of the circle representing each object:
  plot(popnx,popny,cex=2)

  # select a random sample, without replacement, of 10 objects
  # out of the 100 in the population 
  oursample <- sample(1:100,10, replace = FALSE)

  # draw the sample points, in the same plot
  points(popnx[oursample],popny[oursample])

  # distinguish the sample points from the others by color
  points(popnx[oursample],popny[oursample],
       pch=21,bg="red",cex=2)


  # 2.8.2 Sample Estimates
  # The caribou data of Example 2.1 can be entered and 
  # stored as a vector called “y” as follows.
  y <- c(1, 50, 21, 98, 2, 36, 4, 29, 7, 15, 86, 10, 21, 5, 4)

 # The sample mean is 
  mean(y)
 # Can also calculate the mean as.... 
  sum(y)/length(y)

  # Expanding that by the population size gives the estimate 
  # of the population total.
  N <- 286 #total number of possible sample units for this 'population'
  N * mean(y)

  # The calculations for Example 2.1 along with the output looks like this:
  y <- c(1, 50, 21, 98, 2, 36, 4, 29, 7, 15, 86, 10, 21, 5, 4)
  # the sample mean:
  mean(y) #25.93333
  # the sample variance:
  var(y) #919.0667
  # the estimate of the variance of the sample mean:
  (1-15/286)*var(y)/15 # 58.05759
  # and standard error:
  sqrt(58.06) # 7.619711
  # the estimate of the population total:
  286*25.9333 #7416.924
  # the estimated variance of that estimate: 
  286^{2} * 58.0576 # 4748879 
  # and standard error:
  sqrt(4748879) #2179.192

  
  # 2.8.3 Simulation
  # The effectiveness of a sampling strategy can be 
  # studied through the use of stochastic simulation. 
  # In this method a “population” of N units with 
  # corresponding y-values, as similar as possible   
  # to the type to be studied, is obtained or constructed 
  # by some means and stored in the computer. 
  # Then (i) a sample is selected using the design 
  #  under consideration, such as simple random sampling 
  # with sample size n; and (ii) with the sample data, 
  # an estimate of a population characteristic is obtained. 
  # These two steps are repeated b times, where the number 
  # of runs b is a large number. The b repetitions of the 
  # sampling procedure produce b different samples s, 
  # each of n units, and b corresponding values of the estimate. 
  # The average of these values approximates the expected value 
  # of the estimator under the design. 
  # The mean square error of the b values approximates the mean 
  # square error of the estimator under the design. 
  # With an unbiased strategy, the mean square error is 
  # the same as the variance.
  # The sample mean is used as an estimate of the population mean. 
  # For a population a data set included in R called “trees” 
  # is used. The “tree” data set contains measurements on 31 
  # fir trees, so N = 31 in the simulation. 
  # A simple random sample of n = 10 of these units is selected 
  # using the R function “sample” and the sample mean is 
  # calculated for the n units in the sample using the R function
  # “mean.” Then a simulation is carried out by repeating 
  # the selection and estimation procedure b times using 
  # a loop and storing the results in a vector called ybar. 
  # First b = 6 is used to make sure the simulation is working. 
  # Then b = 10,000 iterations or runs of the simulation are done. 
  # Resulting summary statistics are printed and a histogram 
  # is plotted showing the distribution of y over the many samples.
  # Lines after the symbol “#” are comments, and are included 
  # only for explanation.

  # Print the trees data set. 
  trees
  # The variable of interest is tree volume, which for
  # simplicity we name "y".
  y <- trees$Volume
  # The 31 trees will serve as our "population" for
  # purposes of simulation.
  N <- 31
  # sample size:
  n <- 10
  # Select a simple random sample of n units from 1, 2,..., N.
  s <- sample(1:N, n, replace=FALSE)
  # Print out unit numbers for the 10 trees in the sample: 
  s
  # Print the y-values (volumes) of the sample trees:
  y[s]
  # The sample mean:
  mean(y[s])
  # Select another sample from the population and repeat the
  # estimation procedure:
  s <- sample(1:N, n, replace=FALSE)
  s
  mean(y[s])
  # Compare the estimate to the population mean:
  mu <- mean(y)
  mu
  # Try a simulation of 6 runs and print out the six 
  # values of the estimate obtained, mainly to check
  # that the simlation  procedure
  # has no errors:
  b <- 6
  # Let R know that the variable ybar is a vector: 
  ybar <- numeric(6)
  for (k in 1:b){ 
    s <- sample(1:N,n)
    ybar[k] <- mean(y[s])
  }
  ybar
  # Now do a full-size simulation of 10,000 runs:
  b <- 10000
  for (k in 1:b){
    s <- sample(1:N,n)
    ybar[k] <- mean(y[s])
  }
  # Summarize the properties of the sampling strategy
  # graphically and numerically:
  hist(ybar)
  mean(ybar)
  var(ybar)
  # Compare the variance calculated directly from the
  # simulation above to the formula that applies
  # specifically to simple random sampling  with the
  # v sample mean: 
  (1-n/N)*var(y)/n
  sd(ybar)
  sqrt((1-n/N)*var(y)/n)
  # The mean square error approximated from the
  # simulation should be # close to the variance but
  # not exactly equal, since they are  calculated  
  # slightly differently:mean((ybar - mu)^{2})
