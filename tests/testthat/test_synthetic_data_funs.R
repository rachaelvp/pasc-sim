data <- generate_data(1e3, effect_size = 0.1)


learner_list_glmnet <- list(constant = make_learner(Lrnr_mean),
                            binary = make_learner(Lrnr_glmnet),
                            counting = make_learner(Lrnr_glmnet))
learner_list_fast <- list(constant = make_learner(Lrnr_mean),
                          binary = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10),
                          counting = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10))

learner_list_mean <- list(constant = make_learner(Lrnr_mean),
                          binary = make_learner(Lrnr_mean),
                          counting = make_learner(Lrnr_mean))

system.time({
  dgp_estimate <- DGP_estimation(data, learner_list_fast)
})


system.time({
  data_1 <- generate_synthetic(dgp_estimate)
})

mean(data_1$study_tv$pasc)
mean(data$study_tv$pasc)
