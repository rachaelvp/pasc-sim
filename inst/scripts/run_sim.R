library(future)
library(data.table)

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
  n_runs <- 1e2
  sim_specs <- sim_spec
}

# define est and reporter
ltmle_spec <- make_spec(pascLtmle, params = c())
sum_spec <- make_spec(pascSummary, params = c())

learner_list_glmnet <- list(constant = make_learner(Lrnr_mean),
                            binary = make_learner(Lrnr_glmnet),
                            counting = make_learner(Lrnr_glmnet))
learner_list_fast <- list(constant = make_learner(Lrnr_mean),
                          binary = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10),
                          counting = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10))

learner_list_mean <- list(constant = make_learner(Lrnr_mean),
                          binary = make_learner(Lrnr_mean),
                          counting = make_learner(Lrnr_mean))


synth_est <- SyntheticDGP$new(params = list(learner_list = learner_list_glmnet,
                                            child_n = NULL,
                                            child_nruns = 20,
                                            child_est_specs = list(sum_spec, ltmle_spec)))

est_specs <- list(ltmle_spec, sum_spec, synth_spec)

est_spec <- make_spec(pascLtmle, params = c())
reporter <- ps_Reporter$new(params = c())

# run the sims
results = run_sims(sim_specs, est_specs, reporter, n_runs = n_runs)
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
