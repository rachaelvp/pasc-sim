library(future)


devtools::load_all()
options(future.apply.debug=TRUE)
options(future.globals.onReference = "ignore")
plan(future.callr::callr, workers = 8)

params <- list(n = 1e3)
sim_spec <- make_spec(PascSim,params = params)
est_spec <- make_spec(pascLtmle, params = c())
reporter <- ps_Reporter$new(params = c())
results = run_sims(sim_spec, est_spec, reporter, n_runs = 100)
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
