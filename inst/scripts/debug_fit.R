set.seed(1234)

library(ggplot2)
library(truncnorm)
library(data.table)
library(zoo)
data <- generate_data(1e3, effect_size = 0.1)


learner_list_glmnet <- list(constant = make_learner(Lrnr_mean),
                            binary = make_learner(Lrnr_glmnet),
                            counting = make_learner(Lrnr_glmnet))
# TODO: add interaction terms
# TODO: fix time_since_exposure and last_covid to be in the right time ordering (ie after their variables)
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

# dgp_estimate <- DGP_estimation(data, learner_list_glmnet)
dgp_estimate <- DGP_estimation(data, learner_list_glm_true)
pred_data <- dgp_estimate$fit[[1]]$training_task$internal_data$raw_data
fit <- dgp_estimate$fit[["death"]]
coefs <- coef(fit)

all_preds <- lapply(dgp_estimate$fit, function(fit){

  col <- fit$training_task$nodes$outcome
  task <- make_sl3_Task(pred_data, outcome=col, covariates = fit$params$covariates)
  preds <- fit$predict(task)
  })

all_preds <- as.data.table(all_preds)
pred_dt <- cbind(pred_data[,list(id, period)], all_preds)
long <- melt(pred_dt, id = "period", measure = colnames(all_preds), variable.name = "measure", value.name = "value")
n <- length(unique(pred_dt$id))
summaries <- long[,list(mean=mean(value), se = sd(value)/sqrt(n)),by=list(period, measure)]

pred_dt[,lapply(.SD,mean),by=list(period),.SDcols=colnames(all_preds)]

true_p_cols <- sprintf("p_%s", colnames(all_preds))
keep_cols <- which(true_p_cols%in%names(data$study_tv))
true_p_cols <- true_p_cols[keep_cols]
true_p <- data$study_tv[, true_p_cols, with = FALSE]
all_preds <- all_preds[,keep_cols, with = FALSE]
resids <- all_preds - true_p

data$study_tv[,mean(p_death),by=list(period)]

training_data <- dgp_estimate$fit[[1]]$training_task$internal_data$raw_data
pred_data <- training_data[,list(id, period)]
pred_data <- cbind(pred_data, all_preds)

covid_fit <- dgp_estimate$fit[[which(dgp_estimate$pred_nodes=="covid")]]
training_data <- covid_fit$training_task$internal_data$raw_data
coefs <- coef(covid_fit)
pred_dt <- training_data[,list(id, period)]
pred_dt$pred_covid <- covid_fit$predict()
raw_data <- data$study_tv[,list(id, period, p_covid)]
raw_data[,period:=as.numeric(gsub("t_","",period, fixed = TRUE))]
combined <- merge(pred_dt, raw_data, by=c("id","period"))
ggplot(combined, aes(x=p_covid, y=pred_covid, color=factor(period)))+geom_point()

pasc_fit <- dgp_estimate$fit[[which(dgp_estimate$pred_nodes=="pasc")]]
training_data <- pasc_fit$training_task$internal_data$raw_data
coefs <- coef(pasc_fit)
nz_coefs <- coefs[coefs!=0]
names(nz_coefs) <- rownames(coefs)[which(as.vector(coefs!=0))]

#fit model only to true variables
g = glm(pasc ~ last_covid + metformin + paxlovid, training_data, family=binomial())
summary(g)
pred_dt <- training_data[,list(id, period, pasc)]
pred_dt$pred_pasc <- pasc_fit$predict()
pred_dt$pred_pasc_glm <- predict(g, type="response")
raw_data <- data$study_tv[,list(id, period, p_pasc)]
raw_data[,period:=as.numeric(gsub("t_","",period, fixed = TRUE))]
combined <- merge(pred_dt, raw_data, by=c("id","period"))

# compare pred from glmnet to pred from glm with only true variables
mean((combined$p_pasc - combined$pred_pasc)^2)
mean((combined$p_pasc - combined$pred_pasc_glm)^2)

ggplot(combined, aes(x=p_pasc, y=pred_pasc, color=factor(period)))+geom_point()


