#' PASC Sim Synthetic Data Estimation (this should probably yield a new sim we can sample from and apply the same estimation)
#' @import data.table
#' @import tmle3sim
#' @importFrom digest digest
#' @importFrom R6 R6Class
SyntheticDGP <- R6Class(
  "SyntheticDGP",
  inherit = t3s_Estimator,
  lock_objects = FALSE,
  public = list(
    initialize = function(params) {
      super$initialize(params)
      private$.child_est_specs <- params$child_est_specs
    },

    ###START
    estimate = function() {
      sampled_data <- self$simulation$last_sample
      dgp_estimate <- DGP_estimation(sampled_data, self$params$learner_list)
      private$.dgp_estimate <- dgp_estimate

      n_runs <- self$params$child_nruns
      if(n_runs==0){
        results <- data.table()
        return(results)
      }

      # run child specs and return results
      sim_specs <- self$child_sim_spec()
      est_specs <- self$child_est_specs
      reporter <- ps_Reporter$new(params = c())
      results = run_sims(sim_specs, est_specs, reporter, n_runs = n_runs)
      results <- rbindlist(unlist(results, recursive = FALSE))
      label_names <- c("simulation_name","estimator_name","simulation_uuid","estimator_uuid","runtime","seed")
      child_names <- sprintf("child_%s",label_names)
      setnames(results, label_names, child_names)
      return(results)
    },
    child_sim_spec = function(){
      child_n <- self$params$child_n
      if(is.null(child_n)){
        child_n <- self$n
      }
      sim_params <- list(n = child_n,
                         effect_size = self$simulation$effect_size,
                         parent = self,
                         dgp_estimate = self$dgp_estimate)
      spec <- make_spec(SyntheticData, sim_params)
      return(spec)
    }
  ),
  active = list(
    dgp_estimate = function(){
      return(private$.dgp_estimate)
    },
    child_est_specs = function(){
      return(private$.child_est_specs)
    },
    n = function(){
      return(self$simulation$n)
    }
  ),
  private = list(
    .dgp_estimate = NULL
  )
)

SyntheticData <- R6Class(
  "SyntheticData",
  inherit = t3s_Simulation,
  lock_objects = FALSE,
  public = list(
    initialize = function(params = list(), ...) {
      super$initialize(params)
      private$.parent_estimator <- params$parent
      private$.dgp_estimate <- params$dgp_estimate
    },


    sample = function() {
      synthetic_data <- generate_synthetic(self$dgp_estimate, self$n)
      return(synthetic_data)
    }
  ),
  active = list(
    parent_estimator = function(){
      return(private$.parent_estimator)
    },
    dgp_estimate = function(){
      return(private$.dgp_estimate)
    },
    n = function(){
      self$params$n
    },
    parent_seed = function(){
      return(self$parent_estimator$simulation$seed)
    },
    effect_size = function(){
      return(self$params$effect_size)
    }
  ),
  private = list(
    .dgp_estimate = NULL,
    .parent_estimator = NULL
  )
)

