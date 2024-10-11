#' Feature Selection Cost Functions
#'
#' The Feature Selection framework provides for generic cost functions to be
#' defined. This S3 method allows for model fit/cost to be calculated for
#' appropriately structured objects and requires an input object that is
#' structured appropriately (see above).
#' This functionality is performed internally within the feature selection
#' algorithm and is unlikely to require direct calls from the user.
#'
#' @param x An object of class `feature_select` (`typeof = list`) from a call
#'   call to [featureSelection()].
#' @return The return value is one of: Area Under the Curve (AUC),
#'   Correspondence Correlation Coefficient (CCC), Mean Squared Error (MSE),
#'   R-squared, or Sensitivity + Specificity, for the supplied predictions
#'   relative to the actual values.
#' @author Kirk DeLisle, Stu Field
#' @export
cost <- function(x) UseMethod("cost")


#' Create Model Cost Function Definitions for S3 Methods (internal)
#'
#' Declares a cost function object within the Feature Selection framework.
#' These functions should be called in the "feature_select" 
#' object declaration passed to \code{\link{featureSelection}} with 
#' their defaults in place (see examples). See *Details* for options.
#'
#' These functions create a list object for use by S3 methods called on the
#' `feature_select` object. There are currently 5 cost functions implemented:\cr
#' \itemize{
#'   \item AUC
#'   \item MSE (Mean-Squared Error)
#'   \item CCC (Concordance Correlation Coefficient)
#'   \item R2
#'   \item Sensitivity + Specificity
#' }
#'
#' @note These are a series of internal functions called according to the "cost"
#'   argument of [featureSelection()].
#' @return A list containing:
#' \item{display.name }{The official Display Title to be used by any plot methods
#'   called on the object.}
#' \item{maximize }{Whether the object (and its cost function) should be maximized or
#'   minimized.}
#' @author Kirk DeLisle, Stu Field
#' @noRd
CostFxn_AUC <- function() {
  list(display.name = "AUC", maximize = TRUE ) |>
    add_class("fs_auc")
}


#' Area under the Curve (AUC)
#'
#' @noRd
#' @export
cost.fs_auc <- function(x) {
  run      <- getRun(x)
  fold     <- getFold(x)
  tst_rows <- x$cross.val[[run]][[fold]]$test.rows
  auc      <- caTools::colAUC(x$cross.val[[run]][[fold]]$test.predicts,
                              x$data[tst_rows, x$model.type$response])   # calAUC accepts strings or factors
  invisible(as.numeric(auc))
}


#' Cost Function CCC
#'
#' This is the cost funciton for the Concordance Correlation Coefficient.
#' @note These are a series of internal functions
#'   called according to the "cost" argument of [featureSelection()].
#' @noRd
CostFxn_CCC <- function() {
  list(display.name = "CCC", maximize = TRUE) |>
    add_class("fs_ccc")
}


#' Concordance Correlation Coefficient (CCC)
#'
#' @noRd
#' @export
cost.fs_ccc <- function(x) {
  run      <- getRun(x)
  fold     <- getFold(x)
  tst_rows <- x$cross.val[[run]][[fold]]$test.rows
  cv_obj   <- x$cross.val[[run]][[fold]]
  ccc      <- calcCCC(cv_obj$test.predicts,
                      x$data[tst_rows, x$model.type$response])
  invisible(ccc$rho.c)
}


#' Cost Function MSE
#'
#' This is the cost funciton for the Mean Squared Error
#'
#' @note These are a series of internal functions called
#'   according to the "cost" argument of [featureSelection()].
#' @noRd
CostFxn_MSE <- function() {
  list(display.name = "MSE", maximize = FALSE) |>
    add_class("fs_mse")
}


#' Mean Squared Error (MSE)
#'
#' @noRd
#' @export
cost.fs_mse <- function(x) {
  run      <- getRun(x)
  fold     <- getFold(x)
  tst_rows <- x$cross.val[[run]][[fold]]$test.rows
  cv_obj   <- x$cross.val[[run]][[fold]]
  mse      <- mean((cv_obj$test.predicts-x$data[tst_rows, x$model.type$response])^2)
  invisible(as.numeric(mse))
}


#' Cost Function R2
#'
#' This is the cost funciton for the R2
#'
#' @note These are a series of internal functions called
#'   according to the "cost" argument of [featureSelection()].
#' @noRd
CostFxn_R2 <- function() {
  list(display.name = "R-squared", maximize = TRUE) |>
    add_class("fs_r2")
}


#' R-squared (R2)
#'
#' @noRd
#' @importFrom stats cor.test
#' @export
cost.fs_r2 <- function(x) {
  run      <- getRun(x)
  fold     <- getFold(x)
  tst_rows <- x$cross.val[[run]][[fold]]$test.rows
  cv_obj   <- x$cross.val[[run]][[fold]]
  r2       <- stats::cor.test(cv_obj$test.predicts,
                              x$data[tst_rows, x$model.type$response])$estimate^2
  invisible(as.numeric(r2))
}


#' Cost Function SensSpec
#'
#' This is the cost funciton for the Sensitivity + Specificity
#'
#' @note These are a series of internal functions called
#'   according to the "cost" argument of [featureSelection()].
#' @noRd
CostFxn_SensSpec <- function() {
  list(display.name = "Sensitivity + Specificity", maximize = TRUE) |>
    add_class("fs_sens_spec")
}


#' Sensitivity + Specificity (S+S)
#'
#' @noRd
#' @importFrom libml calc_confusion getStat
#' @export
cost.fs_sens_spec <- function(x) {
  run      <- getRun(x)
  fold     <- getFold(x)
  tst_rows <- x$cross.val[[run]][[fold]]$test.rows
  df       <- data.frame(pred  = x$cross.val[[run]][[fold]]$test.predicts,
                         class = x$data[tst_rows, x$model.type$response])
  # pos.class = 2nd factor level!
  cm <- calc_confusion(truth     = df$class,
                       predicted = df$pred,
                       cutoff    = 0.5,
                       pos.class = levels(df$class)[2L]) |>
    summary()
  getStat(cm, "Sens") + getStat(cm, "Spec")
}
