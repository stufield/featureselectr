#' Set up Cross-Validation Folds
#'
#' This is a S3 method that sets up the internal functions for
#' the cross-validation. Since these are internal only methods,
#' documentation will be minimal.
#'
#' @param x An object of class `"feature_select"` from a call
#' call to [featureSelection()].
#' @author Kirk DeLisle
#' @export
setupCrossVal <- function(x) UseMethod("setupCrossVal")


#' Set up Stratified Cross-Validation Folds
#'
#' The *stratified* version is a S3 method that sets up the
#' internal functions for the cross-validation for stratified folds.
#' Stratified implies that the internal structure of the response
#' variable is maintained (e.g. the proportion of disease samples in the
#' fold is comparable to the disease proportion in the full training set).
#'
#' @rdname setupCrossVal
#' @export
setupCrossValStrat <- function(x) UseMethod("setupCrossValStrat")



#' Cross-validation and Bootstrap Setup
#'
#' Create cross-validation folds for each run: non-stratified.
#'
#' @noRd
#' @export
setupCrossVal.feature_select <- function(x) {

  # allow control of random generator
  withr::local_seed(x$random.seed)

  for ( r in 1:x$cross.val$runs ) {
    run              <- sprintf("Run%i", r)
    x$cross.val[[run]] <- list()
    avail.rows       <- seq_along(rownames(x$data)) # remaining rows to choose from
    rem.rows         <- avail.rows
    samples.per.fold <- as.integer(nrow(x$data) / x$cross.val$folds)
    extra.samples    <- nrow(x$data) - samples.per.fold*x$cross.val$folds

    if ( x$cross.val$folds == 1 ) {
      x$cross.val[[run]][["Fold1"]] <- list()
      x$cross.val[[run]][["Fold1"]]$test.rows     <- avail.rows
      x$cross.val[[run]][["Fold1"]]$training.rows <- avail.rows
      next
    }

    for ( f in 1:x$cross.val$folds ) {
      fold      <- sprintf("Fold%s", f)
      x$cross.val[[run]][[fold]] <- list()
      test.rows <- sample(rem.rows, samples.per.fold)
      rem.rows  <- setdiff(rem.rows, test.rows)

      if ( f <= extra.samples ) {
        test.rows <- c(test.rows, sample(rem.rows, 1))
      }

      training.rows <- setdiff(avail.rows, test.rows)
      rem.rows      <- setdiff(rem.rows, test.rows)
      x$cross.val[[run]][[fold]]$test.rows     <- test.rows
      x$cross.val[[run]][[fold]]$training.rows <- training.rows
    }
  }
  invisible(x)
}


#' Cross-validation Setup
#' Create cross-validation folds for each run: stratified.
#' @noRd
#' @export
setupCrossValStrat.feature_select <- function(x) {

  tbl <- table(x$data[, x$strat.column])

  # 1st check if stratification is even possible given class counts
  if ( min(tbl) < x$cross.val$folds ) {
    min <- which.min(tbl)
    err <- sprintf("Class: %s Count: %d Folds: %d",
                   names(tbl)[min], tbl[min],
                   x$cross.val$folds)
    stop(
      "Not enough representative samples per class to stratify: ",
      value(err), call. = FALSE
    )
  }

  # allow control of random generator
  withr::local_seed(x$random.seed)

  # setup Run/Fold data structures
  for ( r in 1:x$cross.val$runs ) {
    for ( f in 1:x$cross.val$folds ) {
      run  <- sprintf("Run%d", r)
      fold <- sprintf("Fold%d", f)
      x$cross.val[[run]]         <- list()
      x$cross.val[[run]][[fold]] <- list()
      x$cross.val[[run]][[fold]]$test.rows     <- numeric(0)
      x$cross.val[[run]][[fold]]$training.rows <- numeric(0)
    }
  }

  for ( n in names(tbl) ) {
    for ( r in 1:x$cross.val$runs ) {
      run              <- sprintf("Run%d", r)
      avail.rows       <- which(x$data[, x$strat.column]==n) # remaining rows to choose from
      rem.rows         <- avail.rows
      samples.per.fold <- floor(length(avail.rows) / x$cross.val$folds)
      extra.samples    <- length(avail.rows) - samples.per.fold * x$cross.val$folds

      for ( f in 1:x$cross.val$folds ) {
        fold <- sprintf("Fold%d", f)

        if ( x$cross.val$folds == 1 ) {
          test.rows     <- rem.rows
          training.rows <- rem.rows
        } else {
          test.rows <- sample(rem.rows, samples.per.fold)
          rem.rows  <- setdiff(rem.rows, test.rows)

          if ( f <= extra.samples ) {
            test.rows <- c(test.rows, sample(rem.rows, 1))
          }

          training.rows <- setdiff(avail.rows, test.rows)
          rem.rows      <- setdiff(rem.rows, test.rows)
        }
        x$cross.val[[run]][[fold]]$test.rows <- c(x$cross.val[[run]][[fold]]$test.rows, test.rows)
        x$cross.val[[run]][[fold]]$training.rows <- c(x$cross.val[[run]][[fold]]$training.rows, training.rows)
      }
    }
  }
  invisible(x)
}
