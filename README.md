
# VAX-PASC sim

Longitudinal Simulation of the effect of vaccine timing on Long Covid
(PASC).

## Assumptions

- time-varying covid and vax are a function of time since last covid or
  vax
- time-varying pasc is a counting process. new cases are a function of
  time between last vax and last covid

## Data Dictionary

- `obs_month` is a binary indicator of any visits occuring in a given
  month

## Modifications

### 11/1/23

- Changed race and data type to factors
- Added vax brand data to both basline (as counts) and time varying (as
  indicators)
- Made metformin a point indicator variable rather than a counting
  process variable
- Modified the sim to increase the probabilities of events:

<!-- -->

    ##   month_covid_prob month_vax_prob final_pasc_prob final_death_prob
    ## 1       0.09257143         0.0385           0.285            0.084

- Changed imputation strategy to 0’s for everything but the counting
  processes (death and covid)
