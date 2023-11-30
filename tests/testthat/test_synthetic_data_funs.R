data <- generate_data(1e3, effect_size = 0.1)
system.time({
  dgp_estimate <- DGP_estimation(data)
})

data_1 <- generate_synthetic(dgp_estimate)

data_100 <- generate_synthetic(dgp_estimate, 1e2)
