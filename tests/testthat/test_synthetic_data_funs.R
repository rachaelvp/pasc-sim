data <- generate_data(1e3, effect_size = 0.1, coarsen = FALSE)


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



system.time({
  dgp_estimate <- DGP_estimation(data, learner_list_glm_true)
})



system.time({
  data_1 <- generate_synthetic(dgp_estimate)
})

pred_data <- dgp_estimate$fit[[1]]$training_task$internal_data$raw_data
to_summarize <- c("covid","pasc","death","vax")
all_preds <- lapply(dgp_estimate$fit[to_summarize], function(fit){

  col <- fit$training_task$nodes$outcome
  task <- make_sl3_Task(pred_data, outcome=col, covariates = fit$params$covariates)
  preds <- fit$predict(task)
})

coef(dgp_estimate$fit[["covid"]])

all_preds <- as.data.table(all_preds)
pred_dt <- cbind(pred_data[,list(id, period, study_days)], all_preds)
pred_dt[, death:=combine_cum_p_val(death), by = list(id)]
pred_dt[, pasc:=combine_cum_p_val(pasc), by = list(id)]
summaries <- calc_summary(list(study_tv = pred_dt))
summaries <- summaries[,c("regime","period","mean","se"), with = FALSE]
summaries[period=="pasc", list(mean)]
summaries[,coarsen:= FALSE]
load("psi_0.Rdata")
test = merge(summaries, psi_0, by = c("regime", "period", "coarsen"))
test[period=="pasc"]
calc_summary(data)[period=="pasc"]
