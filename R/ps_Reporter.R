#' PASC Sim Reporting
#' @import data.table
#' @import tmle3sim
#' @importFrom digest digest
#' @importFrom R6 R6Class
ps_Reporter <- R6Class("ps_Reporter",
                       inherit = t3s_Reporter,
                       public = list(
                         initialize = function(params = NULL, ...){
                           private$.n <- n <- params$n
                           super$initialize(params)
                         },
                         report = function(){
                           super$report()
                         },
                         make_final = function(){
                           result <- super$make_final()
                           result$n <- self$n
                           return(result)
                         }
                       ),
                       active = list(
                         n = function(){
                           return(reporter$simulation$n)
                         },
                         effect_size = function(){
                           return(reporter$simulation$effect_size)
                         }
                       ),
                       private = list(
                         .n = NULL
                       ))
