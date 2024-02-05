devtools::load_all()
psi_results <- list()
n_runs <- 100

# TODO: this should work but not 100%
# more reliable would be to calculate at very large sample sizes, but we have memory issues
for(i in 1:n_runs){
  message(sprintf("run %d", i))
  psi_0 <- calc_psi_0(1e3,effect_size=0.1, coarsen = TRUE, marginal_only = TRUE)
  psi_0_uc <- calc_psi_0(1e3,effect_size=0.1, coarsen = FALSE, marginal_only = TRUE)
  psi_results <- c(psi_results, list(psi_0, psi_0_uc))

  psi_0_all <- rbindlist(psi_results)
  psi_0 <- psi_0_all[, list(mean=mean(mean), se = mean(se)/sqrt(n_runs), effect_size = effect_size[1], n_runs = n_runs), by = list(period, regime, coarsen)]
  setnames(psi_0, c("mean","se"), c("psi_0", "se(psi_0)"))
  save(psi_0,file="psi_0.Rdata")

}
