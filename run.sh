#!/bin/bash
R CMD "devtools::install"
R --no-save < inst/scripts/run_sim_marginal.R
