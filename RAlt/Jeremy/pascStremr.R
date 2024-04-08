#' PASC Sim Synthetic Data Estimation (this should probably yield a new sim we can sample from and apply the same estimation)
#' @import data.table
#' @import tmle3sim
#' @import ltmle
#' @importFrom digest digest
#' @importFrom R6 R6Class
pascStemr <- R6Class(
  "pascStremr",
  inherit = t3s_Estimator,
  lock_objects = FALSE,
  public = list(
    initialize = function(params =  NULL) {
      super$initialize(params)
    },

    ###START
    estimate = function() {

      stop("Stremr not yet implemented")

    }
  )
)
