devtools::load_all()
seed <- 1440925044
params <- list(n = 1e2, effect_size=0.1)
sim_spec = make_spec(PascSim, params)
sim <- PascSim$new(seed = seed, params = params)
est <- pascLtmle$new(params = list())
#est <- pascSummary$new(params = list())
reporter <- ps_Reporter$new(params = c())
sim$estimator <- est
sim$reporter <- reporter
debugonce(est$estimate)
result <- sim$run()
print(result)
