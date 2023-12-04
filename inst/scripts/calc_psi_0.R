results <- list()
n_runs <- 10
for(i in 1:n_runs){
  psi_0 <- calc_psi_0(1e5,effect_size=0.1)
  results <- c(results, list(psi_0))
}
psi_0_all <- rbindlist(results) 
psi_0 <- psi_0_all[, list(mean=mean(mean), se = mean(se)/sqrt(n_runs), effect_size = effect_size[1]), by = list(period, regime)]
setnames(psi_0, c("mean","se"), c("psi_0", "se(psi_0)"))
save(psi_0,file="psi_0.Rdata")