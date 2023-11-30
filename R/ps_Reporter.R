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
                           result$effect_size <- self$effect_size
                           ps <- self$simulation$parent_seed
                           if(!is.null(ps)){
                             result$parent_seed <- ps
                           }

                           private$.final_report <- result
                           return(result)
                         }
                       ),
                       active = list(
                         n = function(){
                           return(self$simulation$n)
                         },
                         effect_size = function(){
                           return(self$simulation$effect_size)
                         }
                       ),
                       private = list(
                         .n = NULL
                       ))
