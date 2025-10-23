
#Chapter 11, problem 1

# Create a data frame of the inputs

  stratum <- c("Stratum 1", "Stratum 2", "Stratum 3")
  N_h <- c(100, 50, 300)
  n_h <- c(50, 50, 50)
  y_bar_h <- c(10, 20, 30)
  s2 <- c(2800, 700, 600)
  N <- sum(N_h)

#Estimate the mean for the whole population
  mu <- (1/N)*((N_h[1]*y_bar_h[1]) + (N_h[2]*y_bar_h[2]) + (N_h[3]*y_bar_h[3]))
  N <- sum(N_h)

# Estimate 95% CI's
  alpha <- 0.05

  #from section 11.3
  df  = ( sum( ((N_h*(N_h-n_h))/n_h) *s2 ))^2 / ((sum(( ((N_h*(N_h-n_h))/n_h) *s2 )^2/(n_h-1))))

  # t distribution
  t <- qt(1-alpha/2, df)
  
  # alternatively, Normal approx
  z <- qnorm(1-alpha/2)

# variance of pop mean  
  var_mu <- ((N_h[1])/N)^2*((N_h[1]-n_h[1])/N_h[1])*(s2[1]/n_h[1]) +
            ((N_h[2])/N)^2*((N_h[2]-n_h[2])/N_h[2])*(s2[2]/n_h[2]) +
            ((N_h[3])/N)^2*((N_h[3]-n_h[3])/N_h[3])*(s2[3]/n_h[3])

  # t distribiution CI's
  lower.CI <- mu-t*sqrt(var_mu)
  lower.CI
  
  upper.CI <- mu+t*sqrt(var_mu)
  upper.CI
  
  
  # z distribiution CI's
  lower.CI <- mu-z*sqrt(var_mu)
  lower.CI
  
  upper.CI <- mu+z*sqrt(var_mu)
  upper.CI

