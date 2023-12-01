vax_probs <- c(0.3, 0.3, 0.2, 0.1, 0.025, 0.025,0.025,0.25)
vax_names <- sprintf("vax%d",seq_along(vax_probs))
no_vax_brand_df <- as.list(rep(0, length(vax_names)))
names(no_vax_brand_df) <- vax_names
no_vax_brand_df <- as.data.table(no_vax_brand_df)


#' DGD Functions
#' @import data.table
#' @importFrom zoo na.locf na.fill0
#' @importFrom truncnorm rtruncnorm
#' @rdname dgd
generate_baseline <- function(n){
  id = seq_len(n)
  ########################################
  #
  # Generate baseline covariates

  pre_visits <- rtruncnorm(n, 0, 1300, 55, 71)
  age <- rtruncnorm(n, 0, 120, 46, 23)
  bmi <- rtruncnorm(n, 0, 80, 27, 22)
  sex <- rbinom(n, 1, 0.6)
  race <- factor(apply(rmultinom(n, 1, c(0.6, 0.18, 0.12, 0.05, 0.05)),2,which.max))
  data_provider <- floor(runif(n,0,100))
  tobacco <- rbinom(n, 1, 0.2)
  obesity <- as.numeric(bmi > 30)
  diabetes <- rbinom(n, 1, 0.2)
  lung_disease <- rbinom(n, 1, 0.2)
  hypertension <- rbinom(n, 1, 0.4)
  depression <- rbinom(n, 1, 0.2)
  corticosteroids <- rbinom(n, 1, 0.3)
  asthma <- rbinom(n, 1, 0.1)


  prior_vax <- floor(runif(n, 0, 4))
  # need to generate brand counts according to prior_vax
  has_vax <- which(prior_vax>0)
  index <- seq_len(n)
  total <- sum(prior_vax)
  vax_index <- rep(index,prior_vax)
  assigned_vaxes <- rmultinom(total, 1, vax_probs)
  rownames(assigned_vaxes) <- vax_names
  vax_brand_df <- data.table(index=vax_index, t(assigned_vaxes))
  vax_brand_df <- merge(data.table(index=index), vax_brand_df, by="index",all.x=TRUE)
  vax_brand_df[is.na(vax_brand_df)] <- 0
  prior_vax_brands <- vax_brand_df[,lapply(.SD,sum), by=list(index)]
  prior_vax_brands$index <- NULL


  community_poverty <- rtruncnorm(n, 0, 0.4, 0.1, 0.04)
  community_social <- rtruncnorm(n, 35, 70, 53, 3)
  data_format <- factor(apply(rmultinom(n, 1, c(0.5, 0.4, 0.05, 0.05)),2,which.max))



  baseline_covariates <- data.table(id, pre_visits, age, bmi, sex, race, data_provider, tobacco, obesity, diabetes, lung_disease, hypertension,
                                    depression, corticosteroids, asthma, prior_vax, prior_vax_brands, community_poverty, community_social, data_format)



  return(baseline_covariates)
}

#' @rdname dgd
generate_tv_init <- function(baseline_covariates){

  # initialize tv variables
  id <- baseline_covariates$id
  n <- nrow(baseline_covariates)
  # generate index vaccine brands
  assigned_vaxes <- rmultinom(n, 1, vax_probs)
  rownames(assigned_vaxes) <- vax_names
  vax_brand_df <- data.table(t(assigned_vaxes))

  tv_covariates <- data.table(id, time = 0, visit = 1, covid = 0,
                              vax = 1, vax_brand_df, pasc = 0,
                              metformin=baseline_covariates$baseline_metformin,
                              paxlovid = 0, death = 0,
                              p_covid = 0, p_vax = 1, p_pasc = 0,
                              p_death = 0, p_visit  = 1)

  # TODO: maybe remove these if not full:
  tv_covariates[,last_vax:=0]
  tv_covariates[,last_covid:=-999]
  tv_covariates[,vax_count:=1]
  tv_covariates[,time_since_exposure:=time - pmax(last_vax, last_covid, na.rm = TRUE)]
  tv_covariates[,covid_lag:=pmax(last_covid-last_vax, 0, na.rm = TRUE)]


  return(tv_covariates)
}

#' @rdname dgd
generate_tv_single <- function(baseline_covariates, current_tv, new_time, regime = NA, limit_covid = TRUE, effect_size){
  tv_covariates <- copy(current_tv)
  n <- nrow(tv_covariates)
  tv_covariates[,time:=new_time]

  # generate COVID
  tv_covariates[,p_covid:=plogis(-10+time_since_exposure/30)]
  if(limit_covid){
    # limit to 1 covid case
    tv_covariates[(last_covid>0), p_covid:=0]
  }
  if(!is.na(regime)){
    tv_covariates[,covid:=as.numeric(regime==time)]
  } else {
    tv_covariates[,covid:=rbinom(n, 1, p_covid)]
  }

  tv_covariates[,last_covid:=ifelse(covid, time, last_covid)]
  tv_covariates[,time_since_exposure:=time - pmax(last_vax, last_covid, na.rm = TRUE)]

  # generate vaccines

  tv_covariates[,p_vax:=plogis(-12+time_since_exposure/30+baseline_covariates$age/50)]

  # limit to at most 5 vaxes
  tv_covariates[,p_vax:=ifelse(vax_count < 5, p_vax, 0)]
  tv_covariates[,vax:=rbinom(n, 1, p_vax)]
  tv_covariates[,vax_count:=vax_count + vax]

  # select a brand for each vax
  # TODO: could modify risk of covid/pasc as a function of vax brand

  total <- sum(tv_covariates$vax)
  if(total==0){
    vax_brand_df <- no_vax_brand_df
  } else {
    assigned_vaxes <- rmultinom(total, 1, vax_probs)
    vax_index <- with(tv_covariates,rep(id,vax))
    rownames(assigned_vaxes) <- vax_names
    vax_brand_df <- data.table(index=vax_index, t(assigned_vaxes))
    vax_brand_df <- merge(data.table(index=tv_covariates$id), vax_brand_df, by="index",all.x=TRUE)
    vax_brand_df[is.na(vax_brand_df)] <- 0
    vax_brand_df$index <- NULL
  }

  set(tv_covariates, NULL, names(vax_brand_df), vax_brand_df)
  tv_covariates[,last_vax:=ifelse(vax, time, last_vax)]
  tv_covariates[,time_since_exposure:=time - pmax(last_vax, last_covid, na.rm = TRUE)]

  # generate meds
  covid_metformin <- ifelse(tv_covariates$covid, rbinom(n, 1, 0.2), 0)
  tv_covariates[,metformin:=as.numeric(baseline_covariates$baseline_metformin | covid_metformin)]
  tv_covariates[,paxlovid:=ifelse(covid, rbinom(n, 1, 0.2), 0)]

  # generate pasc
  #tv_covariates[,covid_lag:=pmax(last_covid-last_vax, 0, na.rm = TRUE)]
  # only consider index vax
  tv_covariates[,covid_lag:=pmax(last_covid, 0, na.rm = TRUE)]
  tv_covariates[,p_pasc:=plogis(-18 + effect_size * covid_lag - 2*metformin - 3*paxlovid)]
  tv_covariates[last_covid<30,p_pasc:=0]

  #only generate pasc if last_covid is at least a month ago
  new_pasc <- with(tv_covariates,rbinom(n, 1, p_pasc))
  tv_covariates[,pasc:=as.numeric(pasc | new_pasc)]

  # generate deaths
  tv_covariates[,p_death:=plogis(-18 + covid*(covid_lag/30 - 2*metformin - 3*paxlovid) + baseline_covariates$age/200)]
  new_death <- with(tv_covariates, rbinom(n, 1, p_death))
  tv_covariates[,death:=as.numeric(death | new_death)]


  # generate visits
  tv_covariates[,p_visit:=pmin(baseline_covariates$visit_rate + 0.8*covid,1)]
  tv_covariates[,visit:=rbinom(n, 1, p_visit)]

    # force visit if pasc or vax
  tv_covariates[,visit:=as.numeric(visit | new_pasc | vax)]

  # elminate post death visits (but keep visit for death)
  tv_covariates[,visit:=as.numeric((visit & !death) | new_death)]


  return(tv_covariates)
}

#' @rdname dgd
generate_tv_all <- function(baseline_covariates, regime,
                            time_increment, end_time, limit_covid, effect_size){


  time <- 0
  times <- seq(from = 0, to=end_time, by=time_increment)
  tv_init <- generate_tv_init(baseline_covariates)
  current_tv <- tv_init
  all_tv <- list(current_tv)
  while(time<end_time){

    # generate covid cases
    time <- time + time_increment
    if(time>end_time){
      break
    }
    current_tv <- generate_tv_single(baseline_covariates, current_tv, time, regime, limit_covid, effect_size)
    all_tv <- c(all_tv, list(current_tv))
  }

  tv <- rbindlist(all_tv, fill = TRUE)

  return(tv)
}

# get the probability of the event at in time in the window
combine_p_val <- function(x){
  1-prod(1-x)
}

#'
format_data <- function(study_tv, baseline_covariates, tv_covars = c(),
                        latent_covars = c("last_vax", "last_covid",
                                          "vax_count", "time_since_exposure",
                                          "covid_lag")){

  # reshape wide
  long <- melt(study_tv, id=c("id","period"))
  wide <- dcast(long, id~period+variable)

  final <- merge(baseline_covariates, wide, by = c("id"))
  all_nodes <- names(final)
  obs_nodes <- grep("\\_p\\_", all_nodes, invert = TRUE, value = TRUE)

  # define nodes
  Wnodes <- setdiff(names(baseline_covariates), c("id","baseline_metformin","visit_rate"))

  covid_nodes <- grep("t_[[:digit:]]+_covid$", obs_nodes, value = TRUE)

  # drop final covid node
  outcome_period <- max(study_tv$period)
  covid_nodes <- covid_nodes[!grepl(outcome_period, covid_nodes)]

  # grab final obs node
  final_obs <- grep(sprintf('%s.*%s', outcome_period, "obs_period"), obs_nodes, value = TRUE)
  Anodes <- c(covid_nodes, final_obs)

  L_vars <- setdiff(tv_covars,c(latent_covars,"covid","pasc","death"))
  L_regex <- paste("t_[[:digit:]]+_",L_vars, "$",collapse = "|", sep="")
  Lnodes <- grep(L_regex, obs_nodes, value = TRUE)
  Lnodes <- setdiff(Lnodes, Anodes)

  Ynodes <- grep(sprintf('%s.*%s', outcome_period, "pasc"), obs_nodes, value = TRUE)

  Cnodes <- grep("death", obs_nodes, value = TRUE)

  node_list <-list (W = Wnodes, A = Anodes, L=Lnodes, Y=Ynodes, C=Cnodes)

  result <- list(final = final, node_list = node_list, study_tv = study_tv)

  return(result)
}
#' @rdname dgd
generate_data <- function(n=1e3, full = TRUE, include_p = TRUE, regime = NA, time_increment = 6, end_time = 20*30, obs_scale = 30, limit_covid = TRUE, effect_size = 1/30, ...){
  baseline_covariates <- generate_baseline(n)
  baseline_obs <- copy(baseline_covariates)

  # hidden baseline variables
  # TODO: hide these in the final output
  baseline_covariates[,baseline_metformin:=rbinom(n, 1, 0.05 + 0.25*diabetes)]
  baseline_covariates[,visit_rate:= pre_visits * time_increment/ 10000]



  tv <- generate_tv_all(baseline_covariates, regime, time_increment, end_time, limit_covid, effect_size)



  ########################################
  #
  # Generate coarsening
  # bin data to month
  tv[,month:=ceiling(time/obs_scale)+1]


  # bin final 6 months (outcome period)
  study_months <- floor(end_time/obs_scale)
  tx_period = study_months - 6 + 1
  outcome_period <- sprintf("t_%02d", tx_period + 1)
  tv[, period:=ifelse(month<=tx_period, sprintf("t_%02d", month), outcome_period)]
  table(tv[,list(period)])

  all_periods <- unique(tv$period)

  # only keep weeks where visits occurred and death has not yet occurred
  if(full){
    obs_tv <- tv
    obs_tv[,visit:=1]
  } else {
    obs_tv <- tv[visit==1]
  }



  expected_periods <- data.table(expand.grid(id=baseline_covariates$id, period=all_periods))
  expected_tv <- merge(expected_periods, obs_tv, by=c("id", "period"), all.x = TRUE)
  tv_covars <- c("covid", "vax", vax_names, "pasc", "metformin", "paxlovid", "death")

  latent_covars <- c("last_vax", "last_covid", "vax_count", "time_since_exposure",
                     "covid_lag")

  if(full){
    tv_covars <- c(tv_covars, latent_covars)
  }
  max_or_na <- function(x){
    suppressWarnings(m <- max(x, na.rm = TRUE))
    ifelse(!is.finite(m), NA_real_, m)
  }



  period_tv <- expected_tv[,lapply(.SD, max_or_na), by=list(id, period), .SDcols = c("visit",tv_covars)]



  # impute tv covars
  period_tv[, obs_period:=visit]

  # we want to do locf for counting processes
  imp_locf_covars <- c("pasc","death")

  if(full){
    imp_locf_covars <- c(imp_locf_covars, latent_covars)
  }
  # all other vars we just impute 0's
  imp_zero_covars <- setdiff(tv_covars, imp_locf_covars)
  period_tv_locf <- period_tv[, na.locf(.SD), by=list(id), .SDcols=c("period", "obs_period", imp_locf_covars)]
  period_tv_zero <- period_tv[, na.fill0(.SD, 0), by=list(id), .SDcols=c("period", "obs_period", imp_zero_covars)]
  study_tv <- merge(period_tv_zero, period_tv_locf, by=c("id","period","obs_period"))

  if(include_p){
    tv_pvals <- grep("^p\\_",names(obs_tv), value=TRUE)
    period_p <- tv[,lapply(.SD, combine_p_val), by=list(id, period), .SDcols = tv_pvals]
    study_tv <- merge(study_tv, period_p, by=c("id","period"))
  } else{
    tv_pvals <- c()
  }
  # we observe the outcome if we observe the final period OR we observe PASC prior
  study_tv[period==outcome_period, obs_period := as.numeric(obs_period | pasc)]

  ########################################
  #
  # Generate final wide dataset
  results <- format_data(study_tv, baseline_covariates, tv_covars, latent_covars)
  results$obs_tv <- obs_tv
  return(results)
}

calc_summary <- function(data){
  study_tv <- data$study_tv
  n <- length(unique(study_tv$id))
  to_summarize <- c("covid","pasc","death","vax","metformin","paxlovid")

  summaries <- study_tv[,lapply(.SD, mean), by=list(period), .SDcols=to_summarize]
  summaries[,regime:=c(0, (seq_len(16-1)-1) * 30 + 6)]
  long <- melt(summaries, id="regime", measure=to_summarize, variable.name = "period", value.name="mean")
  se_p <- function(x,n){
    x <- pmin(pmax(x,1/n),1-(1/n))
    sqrt(x*(1-x))/sqrt(n)
  }
  long[,se:=se_p(mean, n)]

  return(long)
}

calc_psi_0 <- function(n = 1e3, effect_size = 0.1){
  # get A times
  data_1 <- generate_data(n, effect_size = effect_size)
  summary <- calc_summary(data_1)
  period_times <- data_1$obs_tv[,c("time","period")]
  intervention_times <- period_times[, list(time=min(time)), by=list(period)]$time

  # drop the last time (followup)
  intervention_times <- intervention_times[-length(intervention_times)]
  intervention_times[1] <- NA # no intervention
  int_time = intervention_times[1]

  # get p_pasc in final period and 1-6 months post intervention
  final_period <- max(period_times$period)
  all_results <- list()
  for(int_time in intervention_times){

    print(int_time)
    post_intervention_min <- int_time + 30
    post_intervention_max <- int_time + 30*6

    int_data <- generate_data(n, full = TRUE, include_p = TRUE, regime = int_time, effect_size = effect_size)
    final_p <- int_data$obs_tv[period==final_period,list(p=combine_p_val(p_pasc), period = "final"),by=id]
    post_p <- int_data$obs_tv[between(time, int_time + 30, int_time+30*6),list(p=combine_p_val(p_pasc), period = "post"),by=id]
    all_p <- int_data$obs_tv[,list(p=combine_p_val(p_pasc), period = "all"),by=id]

    combined <- rbind(final_p, post_p, all_p)
    combined$regime = int_time
    all_results <- c(all_results, list(combined))
  }

  results <- rbindlist(all_results)
  tsms <- results[,list(mean = mean(p), se = sd(p)/sqrt(.N)),by=list(regime, period)]

  # dcast(tsms, regime~period, value.var="mean")
  # fit msms
  final_msm <- glm(p~regime, results[period=="final"], family=quasibinomial())
  final_beta <- data.frame(regime = "msm", period = "final", summary(final_msm)$coefficients[2, 1:2, drop = FALSE])
  post_msm <- glm(p~regime, results[period=="post"], family=quasibinomial())
  post_beta <- data.frame(regime = "msm", period = "post", summary(post_msm)$coefficients[2, 1:2, drop = FALSE])
  all_msm <- glm(p~regime, results[period=="all"], family=quasibinomial())
  all_beta <- data.frame(regime = "msm", period = "all", summary(all_msm)$coefficients[2, 1:2, drop = FALSE])

  psi_0 <- rbind(tsms, final_beta, post_beta, all_beta, summary, use.names = FALSE)
  psi_0[,effect_size:=effect_size]
  return(psi_0)
}
