
library(data.table)
library(dplyr)
library(glmnet)
library(R6)
library(sl3)
library(tmle3)
library(hal9001)

rm(list = ls())
source("uhal_Alan.R")
source("SimSynFunctions_Alan.R")




# Initiate data, get true values
set.seed(123)
df <- generate_data_simple(N=500)
  node_list <- list(
  W = setdiff(names(df), c("A", "Y")),
  A = "A",
  Y = "Y"
)

# Get True Values

psiP0 <- True_values()
## tmle3 and SL specs
ate_spec <- tmle_ATE(
  treatment_level = 1,
  control_level = 0
)

# choose base learners
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_xgb <- make_learner(Lrnr_xgboost)
lrnr_earth <- make_learner(Lrnr_earth)
lrnr_glm <- make_learner(Lrnr_glm)

# define metalearners appropriate to data types
ls_metalearner <- make_learner(Lrnr_nnls)
lb_metalearner <- make_learner(Lrnr_solnp,
                               learner_function = metalearner_logistic_binomial,
                               loss_function = loss_loglik_binomial)
sl_Y <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_glm, lrnr_earth),
  metalearner = ls_metalearner
)
sl_A <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_glm, lrnr_earth),
  metalearner = lb_metalearner
)
learner_list <- list(A = sl_A, Y = sl_Y)

# HAL spec  
lrnr_uhal <- Lrnr_uhal9001$new()


# Set up Sim
## Specs for SL and HAL and TMLE 

# B is number of simulations
tmle.res <- NULL
simp.res <- NULL
B<- 400
library(foreach)
library(doParallel)
num_cores <- 7
registerDoParallel(cores = num_cores)
foreach(b = 1:B) %do% {
  cat(" b = ",b,"\n")
  
# Simulate Data
df <- generate_data_simple(N=500)
# Fit HAL Q and g
res_Qg <- fit_uhal_Qg(df = df, 
                      y_type = "continuous", 
                      covars = node_list$W)
# Fit SL Q and g
res_Qg_SL <- fit_SL_Qg(df = df, 
                      y_type = "continuous", 
                      covars = node_list$W)

# Get HAL-generated synthetic data
df_syn <- generate_uhal_data(n = nrow(df), 
                             df = df, 
                             y_type = "continuous", 
                             g_fit = res_Qg$g_fit, 
                             Q_fit = res_Qg$Q_fit)

# Get SL-generated synthetic data
df_syn_SL <- generate_SL_data(n = nrow(df), 
                             df = df, 
                             y_type = "continuous", 
                             g_fit = res_Qg$g_fit, 
                             Q_fit = res_Qg$Q_fit)

# Get the empirical estimates of simple parameters
simple.df <- get.estimates(df)
simple.df.hal <- get.estimates(df_syn)
simple.df.SL <- get.estimates(df_syn_SL)


# estimate ATE with original data
tmle_fit_org <- tmle3(ate_spec, df, node_list, learner_list)

# estimate ATE with synthetic HAL data
tmle_fit_hal <- tmle3(ate_spec, df_syn, node_list, learner_list)

# estimate ATE with synthetic HAL data
tmle_fit_SL <- tmle3(ate_spec, df_syn_SL, node_list, learner_list)

# Consolidate into a output objects
simp.res <- rbind(simp.res,c(simple.df$aver,simple.df.hal$aver,simple.df.SL$aver,simple.df$reg[,1],simple.df$reg[,2],simple.df.hal$reg[,1],simple.df.hal$reg[,2],simple.df.SL$reg[,1],simple.df.SL$reg[,2]))

tmle.res <- rbind(tmle.res,c(tmle_fit_org$summary$tmle_est,tmle_fit_org$summary$se,tmle_fit_hal$summary$tmle_est,tmle_fit_hal$summary$se,tmle_fit_SL$summary$tmle_est,tmle_fit_SL$summary$se))

}
save.image("April7.rdata")

