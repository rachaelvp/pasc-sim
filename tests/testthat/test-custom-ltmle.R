data <- generate_data(1e3, effect_size = 0.1)


learner_list_fast <- list(constant = make_learner(Lrnr_mean),
                          binary = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10),
                          counting = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10))

dgp_estimate <- DGP_estimation(data, learner_list_fast)

if(is.null(n)){
  n <- dgp_estimate$n
}

fits <- dgp_estimate$fit
periods <- dgp_estimate$times
pred_nodes <- dgp_estimate$pred_nodes
pred_types <- dgp_estimate$pred_types

# hardcode ltmle for now
# TODO: generalize

final<-data$final
study_tv<-data$study_tv
full<-TRUE
nodes <- data$node_list

##Repeated Data for non_jumping process
#First, get the baseline information
baseline_data<-final[,c("id",nodes$W), with=FALSE]





# Use the long formatted data, which includes patients' history up till and including death
# don't model certain nodes
# TODO: make argument
tv_nodes <- c("id", "period", "obs_period", "covid", "vax", "metformin",
              "paxlovid", "pasc", "death", "last_vax", "last_covid", "vax_count",
              "time_since_exposure")
study_tv_used <- study_tv[,tv_nodes, with = FALSE]
study_tv_used$period<-as.numeric(gsub("t_","",study_tv_used$period))

# add history covariates (lagged from last two timepoints)
pred_data <- add_history(study_tv_used, baseline_data)

# define interventions
id_cols <- c("id","period")
int_cols <- c("covid","death")
obs_cols <- sprintf("obs_%s", int_cols)
setnames(pred_data, int_cols, obs_cols)
ids <- unique(pred_data$id)
intervention_dt <- as.data.table(expand.grid(id = ids, period=periods, d=periods[-length(periods)]))
intervention_dt[,covid:=as.numeric(d==period)]
intervention_dt[,death:=0]
cf_data <- merge(pred_data, intervention_dt, by=id_cols)

# determine which observations match which interventions
match_int = function(.SD, int_cols, obs_cols){
  int_vals <- .SD[,int_cols, with = FALSE]
  obs_vals <- .SD[,obs_cols, with = FALSE]
  match = apply(int_vals==obs_vals,1,all)
}
cf_data[,match_int := match_int(.SD, int_cols, obs_cols)]
ordered_cumprod = function(match_int, period){
  o = order(period)
  cp = cumprod(match_int[o])
  cp[order(o)]
}
cf_data[,match_all := ordered_cumprod(match_int, period), by = list(id, d)]
cf_data[d==11&id==1, list(period, match_int, match_all)]

# generate predictions for g
g_trt_fit <- fits[[which(pred_nodes=="covid")]]
g_task <- make_sl3_Task(cf_data,
                        outcome = g_trt_fit$training_task$nodes$outcome,
                        covariates = g_trt_fit$training_task$nodes$covariates)


g_trt <- g_trt_fit$predict(g_task)
cf_data[,g_trt:=g_trt]
cf_data[,g_ := ordered_cumprod(match_int, period), by = list(id, d)]
g_cens_fit <- fits[[which(pred_nodes=="death")]]
g_task <- make_sl3_Task(cf_data,
                        outcome = g_cens_fit$training_task$nodes$outcome,
                        covariates = g_cens_fit$training_task$nodes$covariates)
g_cens <- g_cens_fit$predict(g_task)
cf_data[,g_cens:=g_cens]
cf_data[,g_all:= g_cens * g_trt]
cf_data[,g_cp := ordered_cumprod(g_all, period), by = list(id, d)]

# TODO: also target individual treatments
h_cols <- c("h_1","h_2")
cf_data[,h_1:=match_all/g_cp]
cf_data[,h_2:=d * match_all/g_cp]

cf_data[d==11&id==1, list(period, match_int, match_all, g_all, g_cp, h_1, h_2)]

# TODO: add bounding
Q_fit <- fits[[which(pred_nodes=="pasc")]]
Q_task <- make_sl3_Task(cf_data,
                        outcome = Q_fit$training_task$nodes$outcome,
                        covariates = Q_fit$training_task$nodes$covariates)
Q_preds <- Q_fit$predict(Q_task)
cf_data[,Q_init:=Q_preds]

period_order <- sort(periods, decreasing = TRUE)
current_period <- period_order[1]
current_data <- cf_data[period==current_period]
current_Q <- current_data$pasc

for(current_period in period_order){
  # do update for t
  current_data <- cf_data[period==current_period]
  current_data[,Q_pred:=current_Q]
  sm_fit <- glm(Q_pred ~ h_1 + h_2 - 1, offset=qlogis(Q_init), current_data, family="binomial")
  sm_covars <- as.matrix(cf_data[,list(1/g_cp, d/g_cp)])
  Q_update <- plogis(qlogis(current_data$Q_init)+sm_covars%*%coef(sm_fit))
  # regression on t-1
  Q_regression <-

Q_task <- make_sl3_Task(current_data,
                        outcome = Q_fit$training_task$nodes$outcome,
                        covariates = Q_fit$training_task$nodes$covariates)
Q <- Q_fit$predict(Q_task)
}

current_period <- tail(periods, 1)



# generate predictions for all d
# TODO: collapse d
current_data <- copy(current_data)



fit <- fits[[node_index]]
col <- pred_nodes[node_index]
type <- pred_types[node_index]
# message(sprintf("generating predictions for '%s' at time=%s", col, current_period))
# bug in sl3 requires columns to not be all missing
set(pred_data, NULL, col, 0)
task <- make_sl3_Task(pred_data, outcome=col, covariates = fit$params$covariates)
