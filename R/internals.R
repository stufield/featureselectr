# Utilities and internals

getRun  <- function(x) paste0("Run", x$cross.val$current.run)
getFold <- function(x) paste0("Fold", x$cross.val$current.fold)

calcCI95se <- function(x) {
  se <- sd(x) / sqrt(length(x))
  c(lower = (mean(x) - (1.96 * se)),
    mean  = mean(x),
    upper = (mean(x) + (1.96 * se)))
}


#' Check if `feature_select` class object
#' Preforms a check of if object is a feature selection object
#' @noRd
is.feature_select <- function(x) {
  class <- inherits(x, "feature_select")
  nms   <- all(c("data",
                 "candidate.markers",
                 "model.type",
                 "cross.val",
                 "cost",
                 "call",
                 "keep.models",
                 "search.type",
                 "cost.fxn",
                 "runs",
                 "folds") %in% names(x))
  (class && nms)
}


#' Check if Search is Complete
#'
#' Catch for Search not yet performed on the `feature_select` object?
#' Incomplete search object.
#' @return If complete, does nothing. Just a pass through.
#' @noRd
check_complete <- function(x) {
  if ( !x$search.complete &&
      (x$cross.val$current.run + x$cross.val$current.fold) == 0 ) {
    stop(
      "Feature selection not yet been performed on this `feature_select` object.\n",
      "Nothing to do here. Perhaps run `Search()`?", call. = FALSE
    )
  }
}


#' Checks `feature_select` Object
#'
#' Check the `feature_select` object for abnormalities in
#' its construction and incongruencies in models/searches.
#'
#' @param x A `feature_select` class object.
#' @note This function is expected to grow as additional checks/traps are implemented.
#' @noRd
checkFeatureSelect <- function(x) {

  msg <- paste("Looks like you mixed up `model.type` ",
               "and `search.type` in object definition.")

  if ( inherits(x$model.type, c("fs_forward_model", "fs_backward_model") )) {
    stop(msg, call. = FALSE)
  }
  if ( inherits(x$search.type, c("fs_nb", "fs_glm", "fs_lm") )) {
    stop(msg, call. = FALSE)
  }
  if ( !"response" %in% names(x$model.type)) {
    stop("No `response` in `model.type` definition.", call. = FALSE)
  }
  if ( sum(grepl("forward", class(x$search.type))) > 0 &&
       !"max.steps"%in%names(x$search.type) ) {
    stop(
      "No `max.steps` defined ... not compatible with a Forward search.",
      call. = FALSE
    )
  }
}


#' Set up Parallel Processing
#'
#' Performs checks and traps necessary for proper parallel
#' processing via \pkg{parallel} and [parallel::mclapply()].
#' Only supported on Linux systems.
#'
#' @param num.cores Integer. The number of cores to use.
#' @return The numeric value of the number of cores to be used.
#' @seealso [parallel::mclapply()]
#' @importFrom parallel mclapply
#' @noRd
parallelSetup <- function(num.cores) {

  if ( num.cores > 1L ) {                 # Checks for parallel processing
    sys <- R.Version()$system
    if ( grepl("linux|darwin", sys) ) {   # Linux system
      if ( requireNamespace("parallel", quietly = TRUE) ) {
        signal_todo(
          "<<< Using", value(num.cores), "CPUs parallel processing >>>\n",
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
        signal_done("Setting `num.cores = 1` and continuing in serial.")
        num.cores <- 1L
      }
    } else if ( grepl("ming", sys) ) {   # Windows system
      signal_oops("Windows OS detected ... parallel processing is not supported.")
      signal_done("Setting `num.cores = 1L` and continuing in serial.")
      num.cores <- 1L
    } else {
      stop("Unknown operating system type: ", value(sys), call. = FALSE)
    }
  }
  num.cores
}


#' Stratification Checks
#'
#' Check the random stratification of the cross-validation visually.
#' The plot compares the proportion of class 1 (level 1) of each fold
#' by run between the training and test sets, as well as the original data.
#'
#' @param x A `feature_select` class object.
#' @importFrom graphics segments plot axis legend abline
#' @noRd
checkStratification <- function(x) {

  resp  <- x$model.type$response
  tab   <- table(x$data[[resp]])
  runs  <- x$cross.val$runs
  folds <- x$cross.val$folds

  if ( length(tab) == 2L ) { # binary response

    signal_done(
      "Checking stratification of cross-folds for BINARY responses"
    )
    data.prev <- prop.table(tab)[1L]

    dimnms <- list(fold = paste0("Fold", 1:folds), run = paste0("Run", 1:runs))

    tbls <- lapply(c("training", "test"), function(cv) {
                     lapply(1:runs, function(r)
                            sapply(1:folds, function(f)
                                   table(x$data[[resp]][ x$cross.val[[sprintf("Run%s", r)]][[f]][[sprintf("%s.rows", cv)]] ])))
                   }) |>
      setNames(c("training", "test"))

    prev_tables <- lapply(tbls, function(cv) {
                          sapply(cv, function(.x)
                                 apply(.x, 2, function(col) col[1L] / sum(col)))
                  })

    for ( i in 1:length(tbls) ) {
      dimnames(prev_tables[[i]]) <- dimnms
    }

    prev_df   <- lapply(prev_tables, as.numeric)
    plot.vals <- c(do.call("rbind", prev_df))
    l         <- length(plot.vals)
    axs_names <- expand.grid(dimnms, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    axs_names <- paste0(axs_names$fold, "_", axs_names$run)
    plot(c(data.prev, plot.vals),
         ylim = 0:1,
         pch = 21, col = 1, cex = 1.5,
         bg = c(2, rep(c(3, 4), times = length(plot.vals) / 2)), xlab = "",
         main = sprintf("Prevalence of '%s' in Stratified Cross-Folds", names(data.prev)),
         ylab = sprintf("Prevalence: %s", names(data.prev)),
         axes = FALSE, frame.plot = TRUE)
    graphics::segments(seq(2, l, by = 2),
                       prev_df$training,
                       seq(3, l + 1, by = 2),
                       prev_df$test, col = 5)
    graphics::axis(1, seq(2, l, by = 2) + 0.5, las = 2, cex.axis = 0.66, labels = axs_names)
    graphics::axis(2)
    graphics::abline(h = data.prev, col = 2, lty=2, lwd = 1.5)
    graphics::abline(v = seq(1, length(plot.vals), by = 2) + 0.5,
                     col = "gray60", lty = 2, lwd = 1)
    graphics::legend("topleft",
                     legend = c("Full Data", "Training", "Test"),
                     col = 1, pch = 21, pt.bg = 2:4,
                     pt.cex = 1.5, cex = 1)

  } else if ( length(tab) > 2L ) { # regression response
    signal_done(
      "checking stratification of cross-folds for CONTINUOUS-type responses"
    )
    # kirk: please complete here when ready
    signal_oops("CONTINUOUS RESPONSES INCOMPLETE!")
  } else {
    stop("Incompatible levels of the `response` variable", call. = FALSE)
  }
  prev_tables
}
