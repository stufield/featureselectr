# Internal utilities

get_run  <- function(x) paste0("Run", x$cross_val$current_run)

get_fold <- function(x) paste0("Fold", x$cross_val$current_fold)

calc_CI95 <- function(x) {
  se <- sd(x) / sqrt(length(x))
  c(lower = (mean(x) - (1.96 * se)),
    mean  = mean(x),
    upper = (mean(x) + (1.96 * se)))
}

#' Check if Search is Complete
#'
#' Catch for Search not yet performed on the `feature_select` object?
#' Incomplete search object.
#' @return If complete, does nothing. Just a pass through.
#' @noRd
check_complete <- function(x) {
  if ( !x$search_complete &&
      (x$cross_val$current_run + x$cross_val$current_fold) == 0 ) {
    stop(
      "Feature selection not yet been performed on this `feature_select` object.\n",
      "Nothing to do here. Perhaps run `Search()`?", call. = FALSE
    )
  }
}


#' Checks `feature_select` Object
#'
#' Check the `feature_select` object for abnormalities in
#'   its construction and incongruencies in models/searches.
#'
#' @param x A `feature_select` class object.
#' @noRd
check_feature_select <- function(x) {

  msg <- paste("Looks like you mixed up `model_type` ",
               "and `search_type` in object definition.")

  if ( inherits(x$model_type, c("fs_forward_model", "fs_backward_model") ) ) {
    stop(msg, call. = FALSE)
  }
  if ( inherits(x$search_type, c("fs_nb", "fs_lr", "fs_lm") ) ) {
    stop(msg, call. = FALSE)
  }
  if ( !"response" %in% names(x$model_type) ) {
    stop("No `response` in `model_type` definition.", call. = FALSE)
  }
  if ( sum(grepl("forward", class(x$search_type))) > 0 &&
       !"max_steps" %in% names(x$search_type) ) {
    stop(
      "No `max_steps` defined ... not compatible with a Forward search.",
      call. = FALSE
    )
  }
}


#' Set up Parallel Processing
#'
#' Performs checks and traps necessary for proper parallel
#'   processing via \pkg{parallel} and [parallel::mclapply()].
#'   Only supported on Linux systems.
#'
#' @param num_cores `integer(1)`. The number of cores to use.
#'
#' @return `integer(1)`. The number of cores to be used.
#'
#' @importFrom parallel mclapply
#' @noRd
parallel_setup <- function(num_cores) {

  if ( num_cores > 1L ) {                 # Checks for parallel processing
    sys <- R.Version()$system
    if ( grepl("linux|darwin", sys) ) {   # Linux system
      if ( requireNamespace("parallel", quietly = TRUE) ) {
        signal_todo(
          "<<< Using", value(num_cores), "CPUs parallel processing >>>\n",
          "Please ensure you set the number of cores appropriately:\n",
          "  If running MODEL selection algorithm:\n",
          "    parallel processing is on the", add_style$red("*RUNS*"), "\n",
          "  If running PARAMETER search algorithm:\n",
          "    parallel processing is on the cross-validation",
          add_style$red("*FOLDS*")
        )
      } else {
        signal_oops(
          "Did not find the `parallel` pkg installed on your Linux system ..."
        )
        signal_done("Setting `num_cores = 1` and continuing in serial.")
        num_cores <- 1L
      }
    } else if ( grepl("ming", sys) ) {   # Windows system
      signal_oops("Windows OS detected ... parallel processing is not supported.")
      signal_done("Setting `num_cores = 1L` and continuing in serial.")
      num_cores <- 1L
    } else {
      stop("Unknown operating system type: ", value(sys), call. = FALSE)
    }
  }
  num_cores
}
