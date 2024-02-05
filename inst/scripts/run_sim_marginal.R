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
params <- list(n = 1e3, effect_size=0.1, coarsen = FALSE)
sim_spec <- make_spec(PascSim,params = params)
params <- list(n = 1e3, effect_size=0.1, coarsen = TRUE)
sim_spec2 <- make_spec(PascSim,params = params)
sim_specs <- c(sim_spec, sim_spec2)
if(cores<=16){
  n_runs <- cores*2
} else {
  n_runs <- 1e2
  # sim_specs <- c(sim_spec2, sim_spec3)
}

# define est and reporter
sum_spec <- make_spec(pascSummary, params = c())

learner_list_glmnet <- list(constant = make_learner(Lrnr_mean),
                            binary = make_learner(Lrnr_glmnet),
                            counting = make_learner(Lrnr_glmnet))


learner_list_glm_true <- list(obs_period = make_learner(Lrnr_mean),
                              covid = make_learner(Lrnr_glm,
                                                   covariates = c("period_length", "vax_t-1", "time_since_exposure_t-1")),
                              vax = make_learner(Lrnr_glm,
                                                 covariates = c("period_length", "covid", "time_since_exposure_t-1", "age")),
                              metformin = make_learner(Lrnr_glm,
                                                       covariates = c("period_length", "diabetes", "covid")),
                              paxlovid = make_learner(Lrnr_glm,
                                                      covariates = c("period_length", "covid")),
                              pasc = make_learner(Lrnr_glm,
                                                  covariates = c("period_length", "last_covid_t-1", "covid", "metformin", "paxlovid")),
                              death = make_learner(Lrnr_glm,
                                                   covariates = c("period_length", "last_covid_t-1","covid", "metformin", "paxlovid", "age")))



synth_spec <- make_spec(SyntheticDGP,
                        params = list(learner_list = learner_list_glmnet,
                                      learner_description = "glmnet",
                                      child_n = NULL,
                                      child_nruns = 5,
                                      child_est_specs = list(sum_spec)))

synth_spec2 <- make_spec(SyntheticDGP,
                        params = list(learner_list = learner_list_glm_true,
                                      learner_description = "glm_true",
                                      child_n = NULL,
                                      child_nruns = 5,
                                      child_est_specs = list(sum_spec)))

# sim <- sim_spec$create(seed = 303213957)
# sim$uuid
# est <- synth_spec2$create()
# est$uuid
# reporter <- ps_Reporter$new()
# sim$estimator <- est
# sim$reporter <- reporter
# sim$run()

est_specs <- list(sum_spec, synth_spec2)

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
