library(future)


devtools::load_all()
options(future.apply.debug=TRUE)
options(future.globals.onReference = "ignore")
cores <- parallelly::availableCores()/2
cores <- min(cores, 32)
plan(future.callr::callr, workers = cores)

params <- list(n = 1e3, effect_size=0.1)
sim_spec <- make_spec(PascSim,params = params)
est_spec <- make_spec(pascLtmle, params = c())
reporter <- ps_Reporter$new(params = c())
results = run_sims(sim_spec, est_spec, reporter, n_runs = 1e3)
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
