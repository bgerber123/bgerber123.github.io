source("sim.data.logistic.mixed.model.r")

# sim.data.logistic.mixed.model = function(n_groups,
#                                          n_per_group,
#                                          beta_0,
#                                          beta_1,
#                                          beta_2,
#                                          sigma_u)

n_groups =10
n_per_group = 20

beta_0 <- -0.5       # Baseline log-odds
beta_1 <-  0.8       # Effect of continuous covariate X1
beta_2 <- -0.4       # Effect of continuous covariate X2

sigma_u = 1.2

 y = sim.data.logistic.mixed.model(n_groups,
                                   n_per_group,
                                   beta_0,
                                   beta_1,
                                   beta_2,
                                   sigma_u)


 )