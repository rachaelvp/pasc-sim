set.seed(1234)

library(truncnorm)
library(data.table)
library(zoo)

params <- list(n = 1e3)
sim <- PascSim$new(params)
ltmle_est <- pascLtmle$new(params = c())
reporter <- ps_Reporter$new(params = c())
sim$estimator <- ltmle_est
sim$reporter <- reporter
results <- sim$run()
