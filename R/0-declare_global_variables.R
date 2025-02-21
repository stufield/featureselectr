##################################
# Declaring Global Variables:
# This is mostly for passing R CMD checks
# global variables that come from other dependant
# packages, or objects in the data/ directory
# Reference: https://github.com/tidyverse/magrittr/issues/29
##################################
if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables(
    c(".",
      "cost_lower_ci95",
      "cost_mean",
      "cost_upper_ci95",
      "cumul_markers",
      "elim_markers",
      "id",
      "metric",
      "training",
      "test",
      "Feature",
      "step"
    )
  )
}
