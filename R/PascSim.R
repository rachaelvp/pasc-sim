#' PASC TRUE DGD
#' @import data.table
#' @import tmle3sim
#' @importFrom digest digest
#' @importFrom R6 R6Class
#'
PascSim <- R6Class("PascSim",
                    inherit = t3s_Simulation,
                    public = list(
                      initialize = function(params = list(), ...){
                        super$initialize(params, ...)
                      },

                      sample = function(){
                          data <- do.call(generate_data, self$params)

                          return(data)
                        }

                    ),
                   active = list(
                     n = function(){
                       return(self$params$n)
                     }
                   )

)
