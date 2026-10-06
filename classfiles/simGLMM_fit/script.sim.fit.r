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


 library(lme4)
 glmm_fit <- glmer(y ~ x1 + x2 + (1 | group), 
                   data = sim_data, 
                   family = binomial(link = "logit"))
 

summary(glmm_fit)


################
# Fit he model in brms

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE) #requires Rtools being installed
install_cmdstan(cores = parallel::detectCores())

set_cmdstan_path(path = NULL)

brm.fit = brm(formula = y ~ x1 + x2 + (1 | group),  
              data = sim_data, 
              family = bernoulli(link = "logit"),
              warmup = 1000, 
              iter = 5000, 
              chains = 3, 
              cores = 3,
#              threads = threading(3),
              sample_prior = TRUE,
#              backend = "cmdstanr"
)

# save(brm.fit,file="brm.fit.glm")
# load("brm.fit.glm")

# See underlying model
brm.fit$model

# See default priors
get_prior(brm.fit)

# Get values of prior distributions
draws = prior_draws(brm.fit)
head(draws)
plot(density(draws[,1]),lwd=4)

# Note
# The b_Intercept parameter is this mean-centered intercept back-transformed to the original scale of the predictors
# Thus. "intercept' can be ignored
brm.fit$fit
summary(brm.fit)

#Extract 'fixed' and 'random effects'
fixef(brm.fit)

#Trace Plots
mcmc_plot(brm.fit, 
          type = "trace")

#All parameters and stuff
names(brm.fit$fit)

#Posterior Distributions
mcmc_areas(as.matrix(brm.fit),
           pars = names(brm.fit$fit)[c(1,2)],
           prob = 0.95)

#####################
# simpler model

brm.fit.simple = brm(formula = y ~ x1 + x2,  
              data = sim_data, 
              family = bernoulli(link = "logit"),
              warmup = 1000, 
              iter = 5000, 
              chains = 3, 
              cores = 3,
              #              threads = threading(3),
              sample_prior = TRUE,
              #              backend = "cmdstanr"
)


#####################
# fixed effect

brm.fit.fixed = brm(formula = y ~ x1 + x2 + group,  
                     data = sim_data, 
                     family = bernoulli(link = "logit"),
                     warmup = 1000, 
                     iter = 5000, 
                     chains = 3, 
                     cores = 3,
                     #              threads = threading(3),
                     sample_prior = TRUE,
                     #              backend = "cmdstanr"
)



library(loo)

loo_results <- loo(brm.fit, brm.fit.simple,brm.fit.fixed)
print(loo_results)
