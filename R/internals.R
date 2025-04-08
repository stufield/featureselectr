# Internal utilities

get_run  <- function(x) {
  run <- x$cross_val$current_run
  if ( is.null(run) ) {
    stop("Invalid current *run* in `feature_select` object.", call. = FALSE)
  }
  paste0("Run", run)
}

get_fold <- function(x) {
  fold <- x$cross_val$current_fold
  if ( is.null(fold) ) {
    stop("Invalid current *fold* in `feature_select` object.", call. = FALSE)
  }
  paste0("Fold", fold)
}

calc_CI95 <- function(x) {
  mu <- mean(x)
  se <- sd(x) / sqrt(length(x))
  data.frame(lower = (mu - (1.96 * se)),
             mean  = mu,
             upper = (mu + (1.96 * se)))
}

#' Check if Search is Complete
#'
#' Catch for Search not yet performed on the `feature_select` object?
#' Incomplete search object.
#' @return If complete, does nothing. Just a pass through.
#' @noRd
check_complete <- function(x) {
  stopifnot("`x` must be a `feature_select` object." = is_feature_select(x))
  .code <- function(x) {
    paste0("\033[90m", encodeString(x, quote = "`"), "\033[39m")
  }

  if ( !x$search_complete ) {
    obj <- as.character(sys.calls()[[1L]][2L])
    stop(
      signal_oops("Feature selection not yet been performed on this object."),
      signal_info(
        paste0("Nothing to do here. Perhaps run ",
               .code(sprintf("Search(%s)", obj)), "?")
      )
    )
  }
  invisible()
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

  stopifnot("`num_cores` must be an integer value." = is_int(num_cores))

  if ( num_cores > 1L ) {                 # Checks for parallel processing
    sys <- R.Version()$system
    if ( grepl("linux|darwin", sys) ) {   # Linux or MacOS systems
      if ( requireNamespace("parallel", quietly = TRUE) ) {
        signal_info(
          "Using", value(num_cores), "CPU cores for parallel processing."
        )
      } else {
        signal_oops(
          "Did not find the `parallel` pkg installed on your Linux system!"
        )
        signal_info("Setting `num_cores = 1` and continuing in serial.")
        num_cores <- 1L
      }
    } else if ( grepl("ming", sys) ) {   # Windows system
      signal_oops("Parallel processing is not supported on Windows systems.")
      signal_info("Setting `num_cores = 1L` and continuing in serial.")
      num_cores <- 1L
    } else {
      stop("Unknown operating system: ", value(sys), call. = FALSE)
    }
  }
  num_cores
}
