devtools::load_all()
seed <- 1440925044
params <- list(n = 1e2, effect_size=0.1)
sim <- PascSim$new(seed = seed, params = params)
est <- pascLtmle$new(params = list())
reporter <- ps_Reporter$new(params = c())
sim$estimator <- est
sim$reporter <- reporter
result <- sim$run()
print(result)
