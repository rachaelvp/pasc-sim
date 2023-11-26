data <- generate_data(1e3)
system.time({
  dgp_est <- DGP_estimation(data)
})

system.time({
  data_1 <- sim_individual(1, dgp_est)
})
