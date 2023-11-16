#' PASC Sim Synthetic Data Estimation (this should probably yield a new sim we can sample from and apply the same estimation)
#' @import data.table
#' @import tmle3sim
#' @importFrom digest digest
#' @importFrom R6 R6Class
SyntheticData <- R6Class(
  "SyntheticData",
  inherit = t3s_Estimator,
  lock_objects = FALSE,
  public = list(
    initialize = function(params) {
      super$initialize(params)
    },

    ###START
    estimate = function() {
      sampled_data <- self$simulation$last_sample

      #TODO: estimate DGP and store fits in this or the subsequent object
      fit <- generate_data
      return(results)
    }
  )
)

SyntheticDGP <- R6Class(
  "SyntheticDGP",
  inherit = t3s_Simulation,
  lock_objects = FALSE,
  public = list(
    initialize = function(params, fit, parent) {
      super$initialize(params)
      private$.parent_estimator <- parent$uuid
      private$.fit <- fit
    },


    sample = function() {

      result <- self$fit(self$n)

      return(result)
    }
  ),
  active = list(
    fit = function(){
      return(private$.fit)
    },
    parent_estimator = function(){
      return(private$.parent_estimator)
    },
    n = function(){
      self$params$n
    }
  ),
  private = list(
    .fit = NULL,
    .parent_estimator = NULL
  )
)

