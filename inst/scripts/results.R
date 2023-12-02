library(tmle3sim)
library(ggplot2)
devtools::load_all()
library(data.table)

# generate true values
if(!file.exists("psi_0.Rdata")){
  psi_0 <- calc_psi_0(1e4,effect_size=0.1)
  setnames(psi_0, c("mean","se"), c("psi_0", "se(psi_0)"))
  save(psi_0,file="psi_0.Rdata")
} else {
  load("psi_0.Rdata")
}



# results <- load_results("Results_backup/")
results <- load_results("Results/")
results <- rbindlist(results, fill = TRUE)
results <- results[is.na(child_simulation_uuid)]
# TODO: fix effect size for synth data
results[,effect_size:=0.1]

# TODO: fix se calc for summary measures
point_estimates <- c("covid", "pasc", "death", "vax",
                     "metformin", "paxlovid")
# se_p <- function(x,n){
#   x <- pmin(pmax(x,1/n),1-(1/n))
#   sqrt(x*(1-x))/sqrt(n)
# }
# results[period%in%point_estimates,se:=se_p(mean,n)]
results[,regime:=as.character(regime)]
results <- merge( results, psi_0, by = c("period","regime","effect_size"))

results <- results[!is.na(mean)&!is.na(se)]
results[,bias:=mean-psi_0]
results[,var:=(mean-mean(mean))^2, by=list(simulation_name, period, regime, n)]
results[,mse:=(mean-psi_0)^2]
results[,lower:=mean-1.96*se]
results[,upper:=mean+1.96*se]
results[,coverage:=data.table::between(psi_0, lower,  upper)]
results[,ci_length:=upper-lower]
results[,power:=!data.table::between(0, lower,  upper)]

conditions = c("simulation_name","period","regime","n")
metrics = c("bias", "var", "mse", "coverage", "ci_length", "power")
perf <- performance_summary(results, conditions, metrics)



perf_pe <- perf[period%in%point_estimates&metric!="power"]
perf_pe[,regime:=as.numeric(regime)]


ggplot(perf_pe[n==10000], aes(x=regime, y= value,
                             ymin = lower, ymax = upper, linetype=simulation_name))+
  geom_line()+geom_ribbon(alpha=0.2)+facet_grid(metric~period, scales="free_y")



# bias2 <- perf[metric == "bias", list(simulation_name = simulation_name, regime = regime, n=n,
#                                      metric = "bias^2",
#                                      value = value^2, se = se,
#                                      lower = lower^2, upper = upper^2, n_mc = n_mc)]
# perf <- rbind(perf, bias2)
# metrics2 <- metrics
# metrics2[1] <- "bias^2"
# perf <- perf[metric!="bias"]
# perf[,metric:=factor(metric, metrics2)]
perf_tsm <- perf[regime!="msm"&period=="all"]
perf_tsm[,regime:=as.numeric(regime)]
ggplot(perf_tsm, aes(
  x = regime, y = value,
  ymin = lower, ymax = upper, color=factor(simulation_name), fill=factor(simulation_name)
)) +
  geom_ribbon(colour=NA,alpha=0.2) +
  geom_line() +
  theme_bw() +
  xlab("Time") +
  scale_color_discrete("n") +
  scale_fill_discrete("n") +
  facet_wrap(~factor(metric),scales = "free_y")

ggplot(perf_tsm[metric%in%c("bias^2","var","mse")], aes(
  x = regime, y = value, color = metric,
  ymin = lower, ymax = upper
)) +
  geom_line()+facet_wrap(~simulation_name)

perf_msm <- perf[regime=="msm"]
print(perf_msm)

# TODO: quantify errors
mr <-missing_results()
errors <- results[regime=="error"]
table(errors$period)
#

# look at performance relative to sample dist
parent_seeds <- unique(results$parent_seed)
parent_results <- results[seed%in%parent_seeds,
                          list(seed = seed, period = period, n = n,
                               regime = regime, effect_size = effect_size,
                               psi_0 = mean, `se(psi_0)`=se)]
child_results <- results[!is.na(parent_seed),
                         list(seed = parent_seed, period = period,
                              regime = regime,
                              mean = mean, se = se)]
child_results$simulation_name <- "relative"
combined <- merge(parent_results, child_results, by=c("seed","period","regime"))
combined <- combined[!is.na(mean)&!is.na(se)]
combined[,bias:=mean-psi_0]
combined[,var:=(mean-mean(mean))^2, by=list(simulation_name, period, regime, n)]
combined[,mse:=(mean-psi_0)^2]
combined[,lower:=mean-1.96*se]
combined[,upper:=mean+1.96*se]
combined[,coverage:=data.table::between(psi_0, lower,  upper)]
combined[,ci_length:=upper-lower]
combined[,power:=!data.table::between(0, lower,  upper)]

conditions = c("simulation_name","period","regime","n")
metrics = c("bias", "var", "mse", "coverage", "ci_length", "power")
perf <- performance_summary(combined, conditions, metrics)

perf_pe <- perf[period%in%point_estimates&metric!="power"]
perf_pe[,regime:=as.numeric(regime)]


ggplot(perf_pe[simulation_name=="PascSim"], aes(x=regime, y= value,
  ymin = lower, ymax = upper, linetype=factor(n)))+
  geom_line()+geom_ribbon(alpha=0.2)+facet_grid(metric~period, scales="free_y")

test <- combined[period=="covid"&regime==96]
ggplot(test, aes(x=factor(psi_0), y=mean))+geom_violin()

