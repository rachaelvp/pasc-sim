#' PASC Sim Synthetic Data Estimation (this should probably yield a new sim we can sample from and apply the same estimation)
#' @import data.table
#' @import tmle3sim
#' @import ltmle
#' @importFrom digest digest
#' @importFrom R6 R6Class
pascLtmle <- R6Class(
  "pascLtmle",
  inherit = t3s_Estimator,
  lock_objects = FALSE,
  public = list(
    initialize = function(params =  NULL) {
      super$initialize(params)
    },

    ###START
    estimate = function() {
      n <- self$simulation$n
      sampled_data <- self$simulation$last_sample
      node_cols <- intersect(names(sampled_data$final), unlist(sampled_data$node_list))
      data <- sampled_data$final[, node_cols, with = FALSE]
      nodes <- sampled_data$node_list


      Anodes <- nodes$A
      n_A <- length(Anodes)
      regime_1 <- diag(1,n_A)
      regime_1[n_A, ] <- 1
      regimes <- array(rep(as.vector(regime_1), each = n),
                      c(n, n_A, n_A))

      regimes <- regimes[,,-n_A]
      #NB: we must invert death to an indicator of UNCENSORING
      death <- data[,nodes$C, with = FALSE]
      uncensoring <- 1- death
      set(data, NULL, nodes$C, uncensoring)
      intervention_days <- c(0, (seq_len(n_A-2)-1) * 30 + 6)
      summary.measures = array(intervention_days, c(n_A-1, 1, 1))
      dimnames(summary.measures)[2]=list("vax_lag")
      fit <- ltmleMSM(data,
                      Anodes = nodes$A,
                      Cnodes = nodes$C,
                      Lnodes = nodes$L,
                      Ynodes = nodes$Y,
                      survivalOutcome = TRUE,
                      regimes = regimes,
                      working.msm = "Y ~ vax_lag",
                      summary.measures = summary.measures
                      )

      # manually calc TSMs
      g <- fit$cum.g
      g <- g[,dim(g)[2],]
      g[is.na(g)] <- 1 # fix censoring (C=0 makes this irrelevant)
      Q <- fit$Qstar[,,1]
      Y <- unlist(data[, nodes$Y, with = FALSE], use.names = FALSE)
      A_mat <- as.matrix(data[,nodes$A, with = FALSE])
      A_arr <- array(rep(as.vector(A_mat), n_A), c(n, n_A, n_A - 1))
      A <- apply(A_arr==regimes,c(1,3), all)
      C <- uncensoring$t_16_death
      IC <- (A*C/g)*(Y-Q)+scale(Q,scale = FALSE)
      se <- sqrt(colMeans(IC^2)/n)
      tsms <- data.table(regime = intervention_days, period = "all",
                         mean = colMeans(Q), se = se)

      Y%*%A/colSums(A)
      msm_beta <- data.frame("msm", "all", summary(fit)$cmat[2, 1:2, drop = FALSE])
      results <- rbind(tsms, msm_beta, use.names = FALSE)
      return(results)
    }
  )
)
