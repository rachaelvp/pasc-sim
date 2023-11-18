devtools::load_all()
seed <- 1440925044
sim <- PascSim$new(seed = seed, params = list(n=1e3))
est <- pascLtmle$new(params = list())
reporter <- ps_Reporter$new(params = c())
sim$estimator <- est
sim$reporter <- reporter
result <- sim$run()
print(result)
