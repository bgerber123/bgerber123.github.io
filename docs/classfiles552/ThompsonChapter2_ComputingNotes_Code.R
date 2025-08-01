
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
  popnx <- runif(100)
  popny <- runif(100)

  # plot the spatial distribution of the population
  plot(popnx,popny)

  # change the size of the circle representing each object:
  plot(popnx,popny,cex=2)

  # select a random sample, without replacement, of 10 objects
  # out of the 100 in the population
  oursample <- sample(1:100,10)

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

  # Expanding that by the population size gives the estimate
  # of the population total.
  N <- 286
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
  s <- sample(1:N, n)
  # Print out unit numbers for the 10 trees in the sample:
  s
  # Print the y-values (volumes) of the sample trees:
  y[s]
  # The sample mean:
  mean(y[s])
  # Select another sample from the population and repeat the
  # estimation procedure:
  s <- sample(1:N, n)
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


##############
 # Example 4: Simulation of simple random sampling using the ﬁr seedling data as a test population.
 # The ﬁr data from the R library “boot” has ﬁr seedling counts for 50 plots, so our test population
 # has N  50 units. We will simulate the sampling strategy simple random sampling design with sample
 # size n  5, using the sample mean as an estimate of the population mean.
 #In R, load the boot package:

  library(boot)

#Take a look at the test population:
    fir
#  The variable of interest is called “count”. For convenience, call it “y”:
    y <- fir$count

#  Take a look at y for the whole population and get a summary table of its
#  values:

    y
    table(y)

#  Determine the actual population mean. Note that in the ﬁeld with your real population you would not
#  have the privilege of seeing this.
    mean(y)

#   Select a random sample without replacement (simple random sample) of 5 units from the unit labels
#   numbered 1 through 50. A unit label corresponds to a row in the ﬁr data ﬁle.
    s <- sample(1:50,5)

#  Look at the unit labels of the selected sample, then at the y-values associated with those units.
#  Make sure you recognize the meaning of these by ﬁnding them in the ﬁr population ﬁle you printed
#  out above.
    s
    y[s]

#   Now compute the sample mean for just the 5 seedling counts in your sample data. Typically, this
#   will not be equal to the population mean, so there is an error associated with the estimate.
    mean(y[s])

#   Two or more commands like this can be put together on a single line, separated by a semicolon.
#   Repeat the line several times using the up-arrow key and you will see different sample means with
#   different samples.
    s <- sample(1:50,5);mean(y[s])

#    We are already doing a simulation of the sampling strategy. Each time you enter the above command,
#    a simple random sample is selected and an estimate is made from the sample data.
#    So far we have not been saving the values of the sample mean for the dif- ferent samples. We will
#    save them in a vector we’ll call “ybar.” Before starting we tell R to expect ybar to be a vector,
#    though without specifying how long it will be.

    ybar <- numeric()


#    We can put our commands in a “for” loop to get ten simulation runs automati- cally. This says, as
#    the run number k goes from 1, 2,... to 10, for each run do all the commands within the brackets.
    for (k in 1:10){
      s <-sample(1:50,5); ybar[k] < -mean(y[s])
      }

#   On each run, a simple random sample of 5 units is selected from the test popu- lation using simple
#   random sampling. The sample mean for the sample data in that run is stored as the k th component of
#    the vector ybar.
#    Since we only did 10 runs, you can look at the sample mean for each run:
      ybar

#   Now look at the average of those 10 sample means. Probably it is not the same as the actual
#   population mean, but might be fairly close.
    mean(ybar)

#   Since we only did 10 runs, we do not get a very good picture of the sampling distribution of the
#   sample mean with our simple random sampling design.
    hist(ybar)

#   We should get a much better picture of the sampling distribution of our estimator by doing 1000 or
#   more simulation runs:

    ybar <- numeric(1000)
    for (i in 1:1000){s<-sample(1:50,5);ybar[i]<-mean(y[s])} mean(ybar)
    hist(ybar)

#    If you are using a fast computer, you could try it with 10,000 or 100,000 runs, which should result
#    in a nice smooth histogram. (But don’t wait forever. If it is taking too long try “control C” or
#                                 “escape” to stop it.)
#    With a large number of runs, the average value of the estimator over all the runs should be very
#    close to its theoretically expected value. One can assess the bias or expected error by looking at
#    the difference between the average value of the estimator over all the samples and the true test
#    population characteristic.
#    Calling the test population mean “mu”,

    mu <- mean(y)
#    the bias in the strategy revealed by the simulation is
    mean(ybar)-mu
#    With simple random sampling, the sample mean is an unbiased estimator of the population mean, so
#    the above difference should be very close to zero. It would approach exactly zero if we increased the
#    number of simulation runs. Another common measure of how
#    well a sampling strategy is doing is the mean square error. The simulation value of this quantity
#    is

    mean((ybar - mu)^2)

#    A good strategy has small (or no) bias and small mean square error.
#    A simulation study such as this offers usually the most practical approach to assessing how well a
#    certain sampling strategy will work for your study, whether the sample size is adequate, and
#    whether a different sampling design or choice of estimator would work better. In addition, it
#    offers perhaps the best insight into how
#    sampling works and what makes an effective design.

