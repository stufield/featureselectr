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
      "cost.lower.ci95",
      "cost.mean",
      "cost.upper.ci95",
      "cumul.markers",
      "elim.markers",
      "id",
      "metric",
      "AptName",
      "step"
    )
  )
}
