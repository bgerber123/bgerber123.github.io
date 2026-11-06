# Parallel simulation of occupancy data using foreach
# ---------------------------------------------------

# Install packages if needed:
# install.packages(c("foreach", "doParallel", "doRNG"))

library(foreach)
library(doParallel)
library(doRNG)

# ---------------------------------------------------
# Occupancy simulation function
# ---------------------------------------------------
# ---------------------------------------------------
# Parallel setup
# ---------------------------------------------------
# Detect available cores and leave one core free
n_cores <- max(1, parallel::detectCores() - 20)
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Optional: reproducible parallel random number streams
# The seed below controls the entire set of 500 simulations
set.seed(123)

# ---------------------------------------------------
# Run 500 simulation iterations in parallel
# ---------------------------------------------------
n_sims <- 500

sim_results <- foreach(i = 1:n_sims, .combine = "list", .multicombine = TRUE,
                       .packages = c()) %dorng% {
                         occ.simulate.fn(N = 200, J = 5, seed = 1000 + i)
                       }

# ---------------------------------------------------
# Optional: inspect the first simulated dataset
# ---------------------------------------------------
# str(sim_results[[1]])
# head(sim_results[[1]]$y)

# ---------------------------------------------------
# Clean up cluster
# ---------------------------------------------------
stopCluster(cl)

