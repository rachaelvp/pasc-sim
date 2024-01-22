set.seed(1234)

library(truncnorm)
library(data.table)
library(zoo)

params <- list(n = 1e3, effect_size=0.1)
sim <- PascSim$new(params)

sum_spec <- make_spec(pascSummary, params = c())


learner_list_glmnet <- list(constant = make_learner(Lrnr_mean),
                            binary = make_learner(Lrnr_glmnet),
                            counting = make_learner(Lrnr_glmnet))

learner_list_glm_true <- list(obs_period = make_learner(Lrnr_mean),
                              covid = make_learner(Lrnr_glm,
                                                   covariates = c("time_since_exposure_t-1")),
                              vax = make_learner(Lrnr_glm,
                                                 covariates = c("time_since_exposure_t-1", "age")),
                              metformin = make_learner(Lrnr_glm,
                                                       covariates = c("diabetes", "covid")),
                              paxlovid = make_learner(Lrnr_glm,
                                                      covariates = c("covid")),
                              pasc = make_learner(Lrnr_glm,
                                                  covariates = c("last_covid_t-1", "metformin", "paxlovid")),
                              death = make_learner(Lrnr_glm,
                                                   covariates = c("last_covid_t-1","covid", "metformin", "paxlovid", "age")))


synth_spec <- make_spec(SyntheticDGP,
                        params = list(learner_list = learner_list_glmnet,
                                      learner_description = "glmnet",
                                      child_n = NULL,
                                      child_nruns = 20,
                                      child_est_specs = list(sum_spec)))

synth_spec2 <- make_spec(SyntheticDGP,
                         params = list(learner_list = learner_list_glm_true,
                                       learner_description = "glm_true",
                                       child_n = NULL,
                                       child_nruns = 2,
                                       child_est_specs = list(sum_spec)))

synth_est <- synth_spec2$create()
est_specs <- list(sum_spec, synth_spec, synth_spec2)
reporter <- ps_Reporter$new(params = c())
sim$estimator <- synth_est
sim$reporter <- reporter
# debugonce(reporter$make_final)
results <- sim$run()

child_sim <- synth_est$child_simulation()
est <- sum_spec$create()
#est <- pascSummary$new(params = list())
reporter <- ps_Reporter$new(params = c())
child_sim$estimator <- est
child_sim$reporter <- reporter
results <- child_sim$run()
