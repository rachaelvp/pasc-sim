set.seed(1234)

library(truncnorm)
library(data.table)
library(zoo)

learner_list_glmnet <- list(constant = make_learner(Lrnr_mean),
                           binary = make_learner(Lrnr_glmnet),
                           counting = make_learner(Lrnr_glmnet))
learner_list_fast <- list(constant = make_learner(Lrnr_mean),
                           binary = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10),
                           counting = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10))

learner_list_mean <- list(constant = make_learner(Lrnr_mean),
                          binary = make_learner(Lrnr_mean),
                          counting = make_learner(Lrnr_mean))
params <- list(n = 1e3, effect_size=0.1)
sim <- PascSim$new(params)
ltmle_spec <- make_spec(pascLtmle, params = c())
sum_spec <- make_spec(pascSummary, params = c())

synth_est <- SyntheticDGP$new(params = list(learner_list = learner_list_mean,
                                            child_n = 100,
                                            child_nruns = 2,
                                            child_est_specs = list(sum_spec, ltmle_spec)))

est_specs <- list(ltmle_spec, sum_spec, synth_spec)
reporter <- ps_Reporter$new(params = c())
sim$estimator <- synth_est
sim$reporter <- reporter
debugonce(reporter$make_final)
results <- sim$run()

child_sim <- synth_est$child_simulation()
est <- sum_spec$create()
#est <- pascSummary$new(params = list())
reporter <- ps_Reporter$new(params = c())
child_sim$estimator <- est
child_sim$reporter <- reporter
results <- child_sim$run()
