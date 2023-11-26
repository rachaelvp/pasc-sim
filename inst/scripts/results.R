library(tmle3sim)
library(data.table)
library(ggplot2)
devtools::load_all()

# generate true values
if(!file.exists("psi_0.Rdata")){
  psi_0 <- calc_psi_0(1e5,effect_size=0.1)
  setnames(psi_0, c("mean","se"), c("psi_0", "se(psi_0)"))
  save(psi_0,file="psi_0.Rdata")
} else {
  load("psi_0.Rdata")
}

results <- load_results()
results <- rbindlist(results)
results <- merge( results, psi_0, by = c("period","regime","effect_size"))

results <- results[!is.na(mean)&!is.na(se)]
results[,bias:=mean-psi_0]
results[,var:=(mean-mean(mean))^2, by=list(regime,n)]
results[,mse:=(mean-psi_0)^2]
results[,lower:=mean-1.96*se]
results[,upper:=mean+1.96*se]
results[,coverage:=between(psi_0, lower,  upper)]
results[,ci_length:=upper-lower]
results[,power:=!between(0, lower,  upper)]

conditions = c("regime","n")
metrics = c("bias", "var", "mse", "coverage", "ci_length", "power")
perf <- performance_summary(results, conditions, metrics)
bias2 <- perf[metric == "bias", list(regime = regime, n=n, metric = "bias^2",
                                     value = value^2, se = se,
                                     lower = lower^2, upper = upper^2, n_mc = n_mc)]
perf <- rbind(perf, bias2)
metrics2 <- metrics
metrics2[1] <- "bias^2"
perf <- perf[metric!="bias"]
perf[,metric:=factor(metric, metrics2)]
perf_tsm <- perf[regime!="msm"]
perf_tsm[,regime:=as.numeric(regime)]
ggplot(perf_tsm, aes(
  x = regime, y = value,
  ymin = lower, ymax = upper, color=factor(n), fill=factor(n)
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
  geom_line()+facet_wrap(~n)

perf_msm <- perf[regime=="msm"]
print(perf_msm)

# TODO: quantify errors
mr <-missing_results()
errors <- results[regime=="error"]
table(errors$period)
#
