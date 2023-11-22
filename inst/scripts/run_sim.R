library(future)


devtools::load_all()
options(future.apply.debug=TRUE)
options(future.globals.onReference = "ignore")

# use at most 32 cores
cores <- parallelly::availableCores()/2
cores <- min(cores, 32)
plan(future.callr::callr, workers = cores)

# define specs
params <- list(n = 1e3, effect_size=0.1)
sim_spec <- make_spec(PascSim,params = params)
params <- list(n = 1e4, effect_size=0.1)
sim_spec2 <- make_spec(PascSim,params = params)
params <- list(n = 1e5, effect_size=0.1)
sim_spec3 <- make_spec(PascSim,params = params)
sim_specs <- list(sim_spec, sim_spec2, sim_spec3)
if(cores==4){
  sim_specs <- sim_spec
  n_runs <- cores*2
} else {
  n_runs <- 1e3
}
# define est and reporter
est_spec <- make_spec(pascLtmle, params = c())
reporter <- ps_Reporter$new(params = c())

# run the sims
results = run_sims(sim_spec, est_spec, reporter, n_runs = n_runs)
save(results, file = "results.Rdata")

# factory <- function(sim_spec, est_spec, reporter){
#
#   function(){
#   tmle3sim:::run_sim(sim_spec, list(est_spec), reporter,
#           save_individual = TRUE, sim_log = TRUE)
#   }
# }
# to_run <- factory(sim_spec, est_spec, reporter)
# p <- r_bg(tmle3sim:::run_sim,
#           package = TRUE,
#           args = list(sim_spec = sim_spec, est_specs = list(est_spec), reporter = reporter, save_individual = TRUE, sim_log = TRUE))
# p$get_result()
# p$read_all_output()
