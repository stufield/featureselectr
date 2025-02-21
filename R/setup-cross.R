#' Set up Cross-Validation Folds
#'
#' This S3 method sets up the internal the *non-stratified* cross-validation.
#'   Since these are mostly internal methods, documentation is minimal.
#'
#' @param x A `feature_select` class object from a call
#'   call to [feature_selection()].
#'
#' @author Kirk DeLisle
#'
#' @export
setup_cross <- function(x) UseMethod("setup_cross")


#' The *stratified* version is a S3 method that sets up the
#'   internal cross-validation for stratified folds.
#'   Stratified implies that the structure (proportions) of the response
#'   variable is maintained (i.e. the proportion of disease samples in the
#'   fold is comparable to the disease proportion in the full training set).
#'
#' @rdname setup_cross
#' @export
setup_cross_strat <- function(x) UseMethod("setup_cross_strat")


#' Cross-validation and Bootstrap Setup (non-stratified)
#'
#' @importFrom withr local_seed
#' @noRd
#' @export
setup_cross.feature_select <- function(x) {

  # allow control of random generator
  local_seed(x$random_seed)

  for ( r in 1:x$cross_val$runs ) {
    run              <- sprintf("Run%i", r)
    x$cross_val[[run]] <- list()
    avail_rows       <- seq_along(rownames(x$data)) # remaining rows to choose from
    rem_rows         <- avail_rows
    samples_per_fold <- as.integer(nrow(x$data) / x$cross_val$folds)
    extra_samples    <- nrow(x$data) - samples_per_fold * x$cross_val$folds

    if ( x$cross_val$folds == 1L ) {
      x$cross_val[[run]][["Fold1"]] <- list()
      x$cross_val[[run]][["Fold1"]]$test_rows     <- avail_rows
      x$cross_val[[run]][["Fold1"]]$training_rows <- avail_rows
      next
    }

    for ( f in 1:x$cross_val$folds ) {
      fold <- sprintf("Fold%s", f)
      x$cross_val[[run]][[fold]] <- list()
      test_rows <- sample(rem_rows, samples_per_fold)
      rem_rows  <- setdiff(rem_rows, test_rows)

      if ( f <= extra_samples ) {
        test_rows <- c(test_rows, sample(rem_rows, 1L))
      }

      training_rows <- setdiff(avail_rows, test_rows)
      rem_rows      <- setdiff(rem_rows, test_rows)
      x$cross_val[[run]][[fold]]$test_rows     <- test_rows
      x$cross_val[[run]][[fold]]$training_rows <- training_rows
    }
  }
  invisible(x)
}


#' Cross-validation Setup (stratified)
#'
#' @importFrom withr local_seed
#' @noRd
#' @export
setup_cross_strat.feature_select <- function(x) {

  tbl <- table(x$data[, x$strat_column])

  # 1st check if stratification is even possible given class counts
  if ( min(tbl) < x$cross_val$folds ) {
    min <- which.min(tbl)
    err <- sprintf("Class: %s Count: %d Folds: %d",
                   names(tbl)[min], tbl[min],
                   x$cross_val$folds)
    stop(
      "Not enough representative samples per class to stratify: ",
      value(err), call. = FALSE
    )
  }

  # allow control of random generator
  local_seed(x$random_seed)

  # setup Run/Fold data structures
  for ( r in 1:x$cross_val$runs ) {
    for ( f in 1:x$cross_val$folds ) {
      run  <- sprintf("Run%d", r)
      fold <- sprintf("Fold%d", f)
      x$cross_val[[run]]         <- list()
      x$cross_val[[run]][[fold]] <- list()
      x$cross_val[[run]][[fold]]$test_rows     <- numeric(0)
      x$cross_val[[run]][[fold]]$training_rows <- numeric(0)
    }
  }

  for ( n in names(tbl) ) {
    for ( r in 1:x$cross_val$runs ) {
      run        <- sprintf("Run%d", r)
      avail_rows <- which(x$data[, x$strat_column] == n) # remaining rows to choose from
      rem_rows         <- avail_rows
      samples_per_fold <- floor(length(avail_rows) / x$cross_val$folds)
      extra_samples    <- length(avail_rows) - samples_per_fold * x$cross_val$folds

      for ( f in 1:x$cross_val$folds ) {
        fold <- sprintf("Fold%d", f)

        if ( x$cross_val$folds == 1L ) {
          test_rows     <- rem_rows
          training_rows <- rem_rows
        } else {
          test_rows <- sample(rem_rows, samples_per_fold)
          rem_rows  <- setdiff(rem_rows, test_rows)

          if ( f <= extra_samples ) {
            test_rows <- c(test_rows, sample(rem_rows, 1L))
          }

          training_rows <- setdiff(avail_rows, test_rows)
          rem_rows      <- setdiff(rem_rows, test_rows)
        }
        x$cross_val[[run]][[fold]]$test_rows <- c(
          x$cross_val[[run]][[fold]]$test_rows,
          test_rows
        )
        x$cross_val[[run]][[fold]]$training_rows <- c(
          x$cross_val[[run]][[fold]]$training_rows,
          training_rows
        )
      }
    }
  }
  invisible(x)
}
