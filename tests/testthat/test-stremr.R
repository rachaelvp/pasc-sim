set.seed(1234)

library(truncnorm)
library(data.table)
library(zoo)
library(stremr)
sampled_data <- generate_data(1e3)
data <- sampled_data$study_tv
# data
tmle_est <- fit_TMLE(OData, tvals = tvals, intervened_TRT = "TI.set1", Qforms = Qforms, parallel = TRUE)


