#' Feature Selection Cost Functions
#'
#' The Feature Selection framework provides for generic cost functions to be
#'   defined. This S3 method allows for model fit/cost to be calculated for
#'   appropriately structured objects and requires an input object that is
#'   structured appropriately.
#'   This functionality is performed internally within the feature selection
#'   algorithm and is unlikely to require direct user calls.
#'
#' @param x A `feature_select` class object.
#'
#' @return A metric, a numeric value, to be optimized depending
#'   on the type, one of AUC, CCC, MSE, R2, Sens + Spec.
#'
#' @author Kirk DeLisle, Stu Field
#'
#' @export
cost <- function(x) UseMethod("cost")


#' Internals to Create Model Cost Function Objects
#'
#' Create a cost function object within the Feature Selection framework.
#'   These functions should be called in the "feature_select"
#'   object declaration passed to [feature_selection()] with
#'   their defaults in place (see examples). See *Details* for options.
#'
#'   These functions create an object for use by S3 methods called on the
#'   `feature_select` object.
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


#' S3 Area Under the Curve (AUC)
#'
#' @noRd
#' @export
cost.fs_auc <- function(x) {
  run      <- get_run(x)
  fold     <- get_fold(x)
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows
  auc      <- calc_auc(x$data[tst_rows, x$model_type$response],
                       x$cross_val[[run]][[fold]]$test_predicts)
  invisible(auc)
}


#' Cost Function CCC
#'
#' @noRd
cost_ccc <- function() {
  list(display_name = "CCC", maximize = TRUE) |>
    add_class("fs_ccc")
}


#' S3 Concordance Correlation Coefficient (CCC)
#'
#' @noRd
#' @export
cost.fs_ccc <- function(x) {
  run      <- get_run(x)
  fold     <- get_fold(x)
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows
  cv_obj   <- x$cross_val[[run]][[fold]]
  ccc      <- calc_ccc(cv_obj$test_predicts,
                       x$data[tst_rows, x$model_type$response])
  invisible(ccc$rho.c)
}


#' Cost Function MSE
#'
#' @noRd
cost_mse <- function() {
  list(display_name = "MSE", maximize = FALSE) |>
    add_class("fs_mse")
}


#' S3 Mean Squared Error (MSE)
#'
#' @noRd
#' @export
cost.fs_mse <- function(x) {
  run      <- get_run(x)
  fold     <- get_fold(x)
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows
  cv_obj   <- x$cross_val[[run]][[fold]]
  mse      <- mean((cv_obj$test_predicts - x$data[tst_rows, x$model_type$response])^2)
  invisible(as.numeric(mse))
}


#' Cost Function R2
#'
#' @noRd
cost_rsq <- function() {
  list(display_name = "R-squared", maximize = TRUE) |>
    add_class("fs_rsq")
}


#' S3 R-squared (R2)
#'
#' @noRd
#' @importFrom stats cor.test
#' @export
cost.fs_r2 <- function(x) {
  run      <- get_run(x)
  fold     <- get_fold(x)
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows
  cv_obj   <- x$cross_val[[run]][[fold]]
  r2       <- stats::cor.test(cv_obj$test_predicts,
                              x$data[tst_rows, x$model_type$response])$estimate^2
  invisible(as.numeric(r2))
}


#' Cost Function SensSpec
#'
#' @noRd
cost_sens_spec <- function() {
  list(display_name = "Sensitivity + Specificity", maximize = TRUE) |>
    add_class("fs_sens_spec")
}


#' S3 Sensitivity + Specificity (S + S)
#'
#' @noRd
#' @export
cost.fs_sens_spec <- function(x) {
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
  sens + spec
}
