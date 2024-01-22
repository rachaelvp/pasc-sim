#' add history columns
add_history <- function(data, baseline_data){
  # add fake data for timepoints before zero
  # TODO: make argument
  tv_vars <-  c("obs_period", "covid", "vax", "metformin",
                "paxlovid", "pasc", "death", "last_vax", "last_covid", "vax_count",
                "time_since_exposure")
  # TODO: make argument
  tv_vals <- c(0, 0, 0, 0, 0, 0, 0, -999, -999, 0, -999)

  fake_data <- data[period==1]
  tv_mat <- matrix(tv_vals, nrow=nrow(fake_data), ncol=length(tv_vals), byrow = TRUE)
  tv_df <- as.data.frame(as.list(tv_vals))
  names(tv_df) <- tv_vars
  set(fake_data, NULL, tv_vars, tv_df)
  fake_data$period <- NULL
  periods <- as.data.table(expand.grid(id=fake_data$id, period=c(-1, 0)))
  fake_data <- merge(periods, fake_data, by = "id")


  data <- rbind(fake_data, data)

  data_p1 <- copy(data)
  data_p1[,period:=period+1]
  setnames(data_p1, tv_vars, sprintf("%s_t-1", tv_vars))
  data_p2 <- copy(data)
  data_p2[,period:=period+2]
  setnames(data_p2, tv_vars, sprintf("%s_t-2", tv_vars))
  pred_data <- merge(data_p1, data, by=c("id","period"))
  pred_data <- merge(data_p2, pred_data, by=c("id","period"))
  pred_data <- merge(baseline_data, pred_data, by="id")

}
#' Make a sl3 task for estimation or prediction
column_task <- function(pred_data, col){
  col_index <- which(names(pred_data)==col)
  pred_cols <- names(pred_data)[1:(col_index-1)]
  pred_cols <- setdiff(pred_cols, "id")
  task <- make_sl3_Task(pred_data, id = "id", covariates = pred_cols, outcome = col)
  return(task)
}
#' Fit a single column based on all previous columns, excluding id
fit_column <- function(pred_data, col, learner = NULL, counting = FALSE, verbose = TRUE){
  if(verbose){
    message(sprintf("Fitting column `%s` with %s", col, learner$name))
  }
  if(is.null(learner)){
    learner <- Lrnr_mean$new()
  }

  if(counting){
    period_seq <- function(period){seq(from=1, to = max(period) + 1)}
    col_neg <- as.vector(pred_data[,col, with = FALSE]==FALSE)
    periods <- pred_data[col_neg, list(period = period_seq(period)), by = "id"]
    col_order <- names(pred_data)
    pred_final <- merge(pred_data, periods, by = c("id", "period"))
    pred_final <- pred_final[,col_order, with = FALSE]
  } else {
    pred_final <- pred_data
  }

  task <- column_task(pred_final, col)
  fit <- learner$train(task)
  return(fit)
}

# learner_list_glmnet <- list(constant = make_learner(Lrnr_mean),
#                             binary = make_learner(Lrnr_glmnet),
#                             counting = make_learner(Lrnr_glmnet))
#
# learner_list_fast <- list(constant = make_learner(Lrnr_mean),
#                             binary = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10),
#                             counting = make_learner(Lrnr_glmnet, nfolds = 5, nlambda = 10))
#
# learner_list_mean <- list(constant = make_learner(Lrnr_mean),
#                           binary = make_learner(Lrnr_mean),
#                           counting = make_learner(Lrnr_mean))

#'
#' DGP estimation
#' Notes on how to generalize:
#' * assume time ordering of columns
#' * identify baseline covariates
#' * identify tv covariates
#' * generate lagged data
#' * fit models to all history (use sl3)
#'
#' @import glmnet
#' @import sl3
DGP_estimation<-function(data, learner_list){
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


  # determine how to process each node we want to fit
  # TODO: make argument
  # detect_node_types
  pred_nodes <- c("obs_period", "covid", "vax", "metformin",
                  "paxlovid", "pasc", "death")

  # TODO: make these external functions
  constant <- unlist(pred_data[,lapply(.SD, function(x){length(unique(x))==1}), .SDcols = pred_nodes])
  binary <- unlist(pred_data[,lapply(.SD, function(x){all(x%in%c(0,1))}), .SDcols = pred_nodes])

  # counting = binary process that is increasing in time
  counting <- pred_data[,lapply(.SD, function(x){all(diff(x)>=0)}), .SDcols = pred_nodes, by = id]
  counting <- unlist(counting[,lapply(.SD, all), .SDcols = pred_nodes])
  counting <- counting & binary

  pred_types <- rep(NA, length(pred_nodes))
  pred_types[constant] <- "constant"
  pred_types[counting & is.na(pred_types)] <- "counting"
  pred_types[binary & is.na(pred_types)] <- "binary"
  if(any(is.na(pred_types))){
    stop("no supported pred_type detected")
  }

  # TODO: make argument


  node_learners <- all(names(learner_list)%in%pred_nodes)

  # fit nodes
  fit_node <- function(pred_node){
    pred_type <- pred_types[pred_node==pred_nodes]
    if(node_learners){
      learner <- learner_list[[pred_node]]
    } else {
      learner <- learner_list[[pred_type]]
    }
    fit_column(pred_data, pred_node, learner, pred_type=="counting")
  }
  fits <- lapply(pred_nodes, fit_node)
  names(fits) <- pred_nodes

  # return results
  dgp_estimate <- list(fit = fits,
                       pred_nodes = pred_nodes,
                       pred_types = pred_types,
                       baseline_data = baseline_data,
                       times = unique(pred_data$period),
                       n = nrow(baseline_data))


  return(dgp_estimate)
}

#' Sample data from DGP estimate
#' TODO: handle baseline covariates
#' TODO: allow intervention nodes
generate_synthetic <- function(dgp_estimate, n = NULL){
  if(is.null(n)){
    n <- dgp_estimate$n
  }

  fits <- dgp_estimate$fit
  periods <- dgp_estimate$times
  pred_nodes <- dgp_estimate$pred_nodes
  pred_types <- dgp_estimate$pred_types

  baseline_obs <- dgp_estimate$baseline_data
  baseline_data <- baseline_obs[sample(baseline_obs$id, n, replace = TRUE)]
  baseline_data[,id:=seq_len(n)]

  # TODO: make part of dgp_estimate output
  tv_vars <-  c("obs_period", "covid", "vax", "metformin",
                "paxlovid", "pasc", "death", "last_vax", "last_covid", "vax_count",
                "time_since_exposure")
  tv_vars_p1 <- sprintf("%s_t-1", tv_vars)
  tv_vars_p2 <- sprintf("%s_t-2", tv_vars)

  tv_data <- data.table(id=baseline_data$id, period = 1)
  set(tv_data, NULL, tv_vars, NA_real_)

  pred_data <- add_history(tv_data, baseline_data)

  all_synthetic <- list()
  i <- 1
  current_period <- 1
  for(current_period in periods){
    pred_data[,period:=current_period]
    for(node_index in seq_along(pred_nodes)){
      fit <- fits[[node_index]]
      col <- pred_nodes[node_index]
      type <- pred_types[node_index]
      # message(sprintf("generating predictions for '%s' at time=%s", col, current_period))
      # bug in sl3 requires columns to not be all missing
      set(pred_data, NULL, col, 0)
      task <- make_sl3_Task(pred_data, outcome=col, covariates = fit$params$covariates)
      preds <- fit$predict(task)
      # TODO: add sample methods to sl3 learners
      vals <- rbinom(n,1,preds)

      if(type=="counting"){
        last_col <- sprintf("%s_t-1", col)
        last_vals <- unlist(pred_data[,last_col, with = FALSE])
        vals <- vals | last_vals
      }

      set(pred_data, NULL, col, vals)
    }

    # calculate derived values
    # TODO: get these functions from the user
    period_days <- function(period){return((period-1) * 30 + 6)}

    # last_vax
    pred_data[,last_vax:=ifelse(vax, period_days(period), `last_vax_t-1`)]
    pred_data[,last_covid:=ifelse(covid, period_days(period), `last_covid_t-1`)]
    pred_data[,vax_count:=`vax_count_t-1`+vax]
    pred_data[,time_since_exposure:=period_days(period)-pmax(last_vax, last_covid)]
    all_synthetic <- c(all_synthetic, list(pred_data))

    # shift pred data
    new_pred <- copy(pred_data)
    set(new_pred, NULL, tv_vars_p2, NULL)
    setnames(new_pred, tv_vars_p1, tv_vars_p2)
    setnames(new_pred, tv_vars, tv_vars_p1)
    set(new_pred, NULL, tv_vars, NA)
    pred_data <- new_pred
  }

  synthetic_data <- rbindlist(all_synthetic)

  # drop history columns and baseline
  set(synthetic_data, NULL, tv_vars_p1, NULL)
  set(synthetic_data, NULL, tv_vars_p2, NULL)
  baseline_covars <- setdiff(names(baseline_data),"id")
  set(synthetic_data, NULL, baseline_covars, NULL)
  synthetic_data[,period:=sprintf("t_%02d", period)]

  # format data to be like real DGP
  results <- format_data(synthetic_data, baseline_data, tv_vars)
  return(results)
}
