source("sim.data.logistic.model.r")


# Example
sim = sim.data.logistic.model(n <- 50,  
                              beta_0 <- -1,
                              beta_1 <- 2 )


# Now setup for a simulation - fimulate data and model fitting
# Model Parameters
  n = 50
  beta_0 = -1
  beta_1 = 0.5

# Simulation parameters
  n.sim=500
  coef.mat = matrix(NA, nrow=n.sim, ncol=2)


for(i in 1:n.sim){
  
  sim = sim.data.logistic.model(n <- n,  
                                beta_0 <- beta_0,
                                beta_1 <- beta_1 )
  

  fit = glm(sim$y~sim$x,family=binomial(link="logit"))
  coef.mat[i,] = c(coef(fit))
}


plot(density(coef.mat[,1]),lwd=3)
abline(v=sim$parms[[1]])


plot(density(coef.mat[,2]),lwd=3)
abline(v=sim$parms[[2]])

########################################
source("sim.data.logistic.multicollinear.model.r")


# Now simualte data with collinear predictor variables


# Example
sim = sim.data.logistic.multicollinear.model(n <- 50,  
                                             beta_0 <- -1,
                                             beta_1 <- 2,
                                             beta_2 <- -2,
                                             rho_x <- 0.9)


# Now setup for a simulation - fimulate data and model fitting
# Model Parameters
  n = 50
  beta_0 = -1
  beta_1 = 2
  beta_2 = 2
  rho_x = 0.7
  
# Simulation parameters
n.sim=500
coef.mat = matrix(NA, nrow=n.sim, ncol=3)


for(i in 1:n.sim){
  
  # Example
  sim = sim.data.logistic.multicollinear.model(n <- n,  
                                               beta_0 <- beta_0,
                                               beta_1 <- beta_1,
                                               beta_2 <- beta_2,
                                               rho_x <- rho_x)
  
  X = model.matrix(~sim$x[[1]]+sim$x[[2]])
  fit = glm(sim$y~0+X,family=binomial(link="logit"))
  coef.mat[i,] = c(coef(fit))
}


par(mfrow=(c(3,1)))
plot(density(coef.mat[,1]),lwd=3,col=1)
abline(v=sim$parms[[1]])

plot(density(coef.mat[,2]),lwd=3,col=2)
abline(v=sim$parms[[2]])

plot(density(coef.mat[,3]),lwd=3,col=3)
abline(v=sim$parms[[3]])
