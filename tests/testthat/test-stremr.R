set.seed(1234)
setwd("~/Dropbox/l3c/pascsim/")
library(devtools)
load_all()
library(truncnorm)
library(data.table)
library(zoo)
library(stremr)
sampled_data <- generate_data(1e3)
data <- sampled_data$study_tv
nodes <- sampled_data$node_list
baseline <- sampled_data$final[,c("id", nodes$W), with = FALSE]
data <- merge(baseline, data, by="id")
data[,t:=as.numeric(gsub("t_","",period, fixed = TRUE))]
time_nodes <- unique(data$t)
time_nodes <- time_nodes[-length(time_nodes)]

interventions <- sapply(time_nodes, function(x)as.numeric(data$t==x|data$t==16))
interventions <- as.data.table(interventions)
names(interventions) <- sprintf("cf_covid_%02d", time_nodes)
set(data, NULL, names(interventions), interventions)
L_tv <- unique(gsub("t_.*_","", nodes$L))
covars <- c(nodes$W, L_tv)
# TODO: fix vax baseline vs vax TV
covars <- intersect(covars, names(data))
setkeyv(data, c("id","t"))
setDTthreads(1)
stremr:::DataStorageClass$undebug("check_norows_after_event")
OData <- importData(data, ID="id", t="t", covars = covars,
                    CENS = "death", TRT = "covid",
                    MONITOR = "obs_period", OUTCOME = "pasc", remove_extra_rows = FALSE)

print(OData)
OData <- fitPropensity(OData, gform_CENS = "death ~ covid + vax + time_since_exposure",
                       gform_MONITOR = "obs_period ~ covid + vax + time_since_exposure",
                       gform_TRT = "covid ~ vax + time_since_exposure")
wts_all <- lapply(names(interventions),function(int) getIPWeights(OData, intervened_TRT=int))

# dat
tmle_est <- fit_TMLE(OData, tvals = tvals, intervened_TRT = "TI.set1", Qforms = Qforms, parallel = TRUE)

# generate fit for last timepoint conditional on history
# for each intervention, generate intervention-specific predictions and regress on history
# repeat for all previous timepoints
# if interested in MSM over timepoints, repeat for each previous Y node
# fit msm to these predictions

# modification for tmle:
# fit g node for each timepoint
# get product of g's matching intervention
# get derivative for each coefficient for each intervention
# these should just be the summary measures times I(A=a)/g(A)
# generate a matrix with nD rows and nB columns
# fit submodel using this data, one epsilon per beta
# calculate updates
# regress on past timepoint



