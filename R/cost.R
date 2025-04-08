#' Feature Selection Cost Functions
#'
#' The feature selection framework provides for generic cost functions to be
#'   defined. This S3 method allows for model fit/cost to be calculated for
#'   appropriately structured objects and requires an input object that is
#'   structured appropriately.
#'
#' This functionality is performed internally within the feature selection
#'   algorithm and should never require direct user calls.
#'
#' @param x A `feature_select` class object.
#'
#' @return A metric, a scalar double, to be optimized depending
#'   on the type, one of: AUC, CCC, MSE, R2, or Sens + Spec.
#'
#' @importFrom stats cor.test
#' @author Stu Field, Kirk DeLisle
#' @noRd
.cost <- function(x) UseMethod(".cost")

# S3 AUC
.cost.fs_auc <- function(x) {
  run      <- get_run(x)
  fold     <- get_fold(x)
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows
  calc_auc(x$data[tst_rows, x$model_type$response],
           x$cross_val[[run]][[fold]]$test_predicts) |> invisible()
}

# S3 CCC
.cost.fs_ccc <- function(x) {
  run      <- get_run(x)
  fold     <- get_fold(x)
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows
  cv_obj   <- x$cross_val[[run]][[fold]]
  ccc      <- calc_ccc(cv_obj$test_predicts,
                       x$data[tst_rows, x$model_type$response])
  invisible(ccc)
}

# S3 MSE
.cost.fs_mse <- function(x) {
  run      <- get_run(x)
  fold     <- get_fold(x)
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows
  cv_obj   <- x$cross_val[[run]][[fold]]
  mean((cv_obj$test_predicts - x$data[tst_rows, x$model_type$response])^2) |>
    invisible()
}

# S3 R2
.cost.fs_r2 <- function(x) {
  run      <- get_run(x)
  fold     <- get_fold(x)
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows
  cv_obj   <- x$cross_val[[run]][[fold]]
  r2       <- stats::cor.test(cv_obj$test_predicts,
                              x$data[tst_rows, x$model_type$response])$estimate^2
  invisible(r2)
}

# S3 Sensitivity + Specificity
.cost.fs_sens_spec <- function(x) {
  run       <- get_run(x)
  fold      <- get_fold(x)
  tst_rows  <- x$cross_val[[run]][[fold]]$test_rows
  probs     <- x$cross_val[[run]][[fold]]$test_predicts
  classes   <- x$data[tst_rows, x$model_type$response]
  neg_class <- levels(classes)[1L]
  pos_class <- levels(classes)[2L] # 2nd factor level!
  cm <- data.frame(   # confusion matrix
    Truth     = classes,
    Predicted = factor(ifelse(probs >= 0.5, pos_class, neg_class),
                       levels = c(neg_class, pos_class))
    ) |> table()
  tn <- cm[1L, 1L]
  tp <- cm[2L, 2L]
  fn <- cm[2L, 1L]
  fp <- cm[1L, 2L]
  sens <- tp / (tp + fn)
  spec <- tn / (tn + fp)
  invisible(as.double(sens + spec))
}



# Internals ----

#' Create a cost function object within the Feature Selection framework.
#'   These functions should be called in the "feature_select"
#'   object declaration passed to [feature_selection()] with
#'   their defaults in place.
#'
#' These functions create an object with a specific class for
#'   use by downstream S3 methods called on the `feature_select` object.
#'
#' @note These are a series of functions called internally
#'   according to the "cost" argument of [feature_selection()].
#'
#' @return All return a list containing:
#'   \item{display_name }{The official Display Title to be used
#'      by any plot methods called on the object.}
#'   \item{maximize }{Whether the cost function should be maximized.}
#'
#' @noRd
cost_auc <- function() {
  list(display_name = "AUC", maximize = TRUE ) |>
    add_class("fs_auc")
}

cost_ccc <- function() {
  list(display_name = "CCC", maximize = TRUE) |>
    add_class("fs_ccc")
}

cost_mse <- function() {
  list(display_name = "MSE", maximize = FALSE) |>
    add_class("fs_mse")
}

cost_rsq <- function() {
  list(display_name = "R-squared", maximize = TRUE) |>
    add_class("fs_rsq")
}

cost_sens_spec <- function() {
  list(display_name = "Sensitivity + Specificity", maximize = TRUE) |>
    add_class("fs_sens_spec")
}
