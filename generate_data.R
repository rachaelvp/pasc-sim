# Exposure: 7 bins (14 months)
# Outcome: 5 bins (10 months)

set.seed(1234)

library(truncnorm)
library(data.table)
library(zoo)

n <- 1e3
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
vax_probs <- c(0.3, 0.3, 0.2, 0.1, 0.025, 0.025,0.025,0.25)
vax_names <- sprintf("vax%d",seq_along(vax_probs))
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

# hidden baseline variables
baseline_metformin <- rbinom(n, 1, 0.05 + 0.25*diabetes)
visit_rate <- pre_visits / 10000

########################################
#
# Generate time varying data

# time=0 is the index vaccine
time <- 0

# move time forward by a week and generate tv variables until 20 months is reached
time_increment <- 7
study_months <- 20

# TODO: end_time may be less for some patients if their index vax is more recent
end_time <- study_months*30

# initialize tv variables
vax_count <- prior_vax + 1
last_vax <- 0
last_covid <- NA
pasc <- 0
death <- 0
no_vax_brand_df <- as.list(rep(0, length(vax_names)))
names(no_vax_brand_df) <- vax_names
no_vax_brand_df <- as.data.table(no_vax_brand_df)

# generate index vaccine brands
assigned_vaxes <- rmultinom(n, 1, vax_probs)
rownames(assigned_vaxes) <- vax_names
vax_brand_df <- data.table(t(assigned_vaxes))

tv_covariates <- data.table(id, time, visit = 1, covid = 0, vax = 1, vax_brand_df, pasc, metformin=baseline_metformin, paxlovid = 0, death)
all_tv <- list(tv_covariates)


while(time<end_time){
  
  # generate covid cases
  time <- time + time_increment
  time_since_exposure <- time - pmax(last_vax, last_covid, na.rm = TRUE)
  pcovid <- plogis(-10+time_since_exposure/30)
  covid <- rbinom(n, 1, pcovid)
  last_covid <- ifelse(covid, time, last_covid)
  
  # generate vaccines
  time_since_exposure <- time - pmax(last_vax, last_covid, na.rm = TRUE)
  pvax <- plogis(-12+time_since_exposure/30+age/50)
  
  # limit to at most 5 vaxes
  pvax <- ifelse(vax_count < 5, pvax, 0)
  
  # select a brand for each vax
  # TODO: could modify risk of covid/pasc as a function of vax brand
  vax <- rbinom(n, 1, pvax)
  total <- sum(vax)
  if(total==0){
    vax_brand_df <- no_vax_brand_df
  } else {
    assigned_vaxes <- rmultinom(total, 1, vax_probs)
    vax_index <- rep(index,vax)
    rownames(assigned_vaxes) <- vax_names
    vax_brand_df <- data.table(index=vax_index, t(assigned_vaxes))
    vax_brand_df <- merge(data.table(index=index), vax_brand_df, by="index",all.x=TRUE)
    vax_brand_df[is.na(vax_brand_df)] <- 0
    vax_brand_df$index <- NULL
  }  
  last_vax <- ifelse(vax, time, last_vax)
  
  # generate meds
  covid_metformin <- ifelse(covid, rbinom(n, 1, 0.2), 0)
  metformin <- as.numeric(baseline_metformin | covid_metformin)
  paxlovid <- ifelse(covid, rbinom(n, 1, 0.2), 0)
  
  # generate pasc
  # TODO make final pasc prevalence 2-3%
  covid_lag <- pmax(last_covid-last_vax, 0, na.rm = TRUE)
  ppasc <- plogis(-18 + covid_lag/30 - 2*metformin - 3*paxlovid)
  
  #only generate pasc if last_covid is at least a month ago
  new_pasc <- ifelse(!is.na(last_covid)&last_covid>30, rbinom(n, 1, ppasc), 0)
  pasc <- as.numeric(pasc | new_pasc)
  
  # generate deaths
  pdeath <- plogis(-18 + covid*(covid_lag/30 - 2*metformin - 3*paxlovid) + age/200)
  new_death <- rbinom(n, 1, pdeath)
  death <- as.numeric(death | new_death)
  
  
  # generate visits
  pvisit <- visit_rate * time_increment + 0.8*covid
  visit <- rbinom(n, 1, pvisit)
  
  # force visit if pasc or vax
  visit <- as.numeric(visit | new_pasc | vax)
  
  # elminate post death visits (but keep visit for death)
  visit <- as.numeric((visit & !death) | new_death)
  
  # add new tv to dataset 
  tv_covariates <- data.table(id, time, visit, covid, vax, vax_brand_df, pasc, metformin, paxlovid, death)
  all_tv <- c(all_tv, list(tv_covariates))
}

tv <- rbindlist(all_tv)

# only keep weeks where visits occurred and death has not yet occurred
obs_tv <- tv[visit==1]

########################################
#
# Generate monthly coarsening

# bin data to month
obs_tv[,month:=ceiling(time/30)+1]

expected_months <- data.table(expand.grid(id=id, month=seq_len(study_months)))
expected_tv <- merge(expected_months, obs_tv, by=c("id", "month"), all.x = TRUE)
tv_covars <- c("covid", "vax", vax_names, "pasc", "metformin", "paxlovid", "death")
max_or_na <- function(x){
  suppressWarnings(m <- max(x, na.rm = TRUE))
  ifelse(!is.finite(m), NA_real_, m)
}

month_tv <- expected_tv[,lapply(.SD, max_or_na), by=list(id, month), .SDcols = tv_covars]

# impute tv covars
month_tv[, obs_month:=as.numeric(!is.na(covid))]

# we want to do locf for counting processes
imp_locf_covars <- c("pasc","death")
# all other vars we just impute 0's
imp_zero_covars <- setdiff(tv_covars, imp_locf_covars)

month_tv_locf <- month_tv[, na.locf(.SD), by=list(id), .SDcols=c("month", "obs_month", imp_locf_covars)]
month_tv_zero <- month_tv[, na.fill0(.SD, 0), by=list(id), .SDcols=c("month", "obs_month", imp_zero_covars)]
month_tv <- merge(month_tv_zero, month_tv_locf, by=c("id","month","obs_month"))

# bin final 6 months (outcome period)
tx_period = study_months - 6
outcome_period <- sprintf("t_%02d", tx_period + 1)
study_tv <- month_tv
study_tv[, period:=ifelse(month<=tx_period, sprintf("t_%02d", month), outcome_period)]
study_tv <- study_tv[,lapply(.SD, max_or_na), by=list(id, period), .SDcols = c("obs_month",tv_covars)]

# we observe the outcome if we observe the final month OR we observe PASC prior
study_tv[period==outcome_period, obs_month := as.numeric(obs_month | pasc)]

########################################
#
# Generate final wide dataset

# reshape wide
long <- melt(study_tv, id=c("id","period"))
wide <- dcast(long, id~period+variable)

final <- merge(baseline_covariates, wide, by = c("id"))

# define nodes
Wnodes <- setdiff(names(baseline_covariates), "id")

covid_nodes <- grep("covid", names(final), value = TRUE)

# drop final covid node
covid_nodes <- covid_nodes[!grepl(outcome_period, covid_nodes)]

# grab final obs node
final_obs <- grep(sprintf('%s.*%s', outcome_period, "obs_month"), names(final), value = TRUE)
Anodes <- c(covid_nodes, final_obs)

L_vars <- setdiff(tv_covars,c("pasc","death"))
L_regex <- paste("t_.*",L_vars, collapse = "|", sep="")
Lnodes <- grep(L_regex, names(final), value = TRUE)

Ynodes <- grep(sprintf('%s.*%s', outcome_period, "pasc"), names(final), value = TRUE)

Cnodes <- grep("death", names(final), value = TRUE)

node_list <-list (W = Wnodes, A = Anodes, L=Lnodes, Y=Ynodes, C=Cnodes)

save(final, node_list, file="gendata.Rdata")
write.csv(final, "final_data.csv", row.names = FALSE)
write.csv(obs_tv, "raw_tv_data.csv", row.names = FALSE)
