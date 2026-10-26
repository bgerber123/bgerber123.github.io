source("occ.simulate.fn.r")
library(unmarked)

sim <- occ.simulate.fn(N = 150, J = 4, seed = 123)

sim$X = data.frame(site.cov = sim$X)

sim.UMF <- unmarkedFrameOccu(sim$y,
                             siteCovs = sim$X
                             )
plot(sim.UMF, panels=1)

fit.null = occu(~1~1, data=sim.UMF)
summary(fit.null)

fit.sim.model = occu(~site.cov~site.cov, data=sim.UMF)
summary(fit.sim.model)



#############################
library(ubms)
library(loo)
source("occ.rand.int.simulate.fn.r")
sim.rand.int <- occ.rand.int.simulate.fn(N = 150, J = 4, seed = 123)

sim.rand.int$X = data.frame(site.cov = sim.rand.int$X)

sim.rand.int.UMF <- unmarkedFrameOccu(sim$y)

siteCovs(sim.rand.int.UMF) = list(site.cov=sim$X,
                                  site.id = factor(seq_len(nrow(sim$y)))
                                  )
basic.model <- stan_occu(~ site.cov ~ site.cov, 
                         data = sim.rand.int.UMF)

basic.model


fit.rand.int <- stan_occu(~ site.cov + (1 | site.id) ~ site.cov + (1 | site.id), 
                   data = sim.rand.int.UMF)

fit.rand.int
