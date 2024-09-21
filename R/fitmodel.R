#' Model Fitting Methods of Various Forms
#'
#' Provides linear regression, Naive Bayes, and logistic regression
#' functionality within the Feature Selection framework. The model type object
#' requires an input object that is structured appropriately (see above).
#' This functionality is performed internally within the feature selection
#' algorithm and is unlikely to require direct calls from the user.
#'
#' The Feature Selection framework provides for generic model types to be
#' defined. This S3 method allows for model types to be fit for appropriately
#' structured objects. The model input object *must* be structured
#' appropriately (see above). Note the inclusion of a `frmla` argument
#' with the desired formula. Without this and in the correct form, the method
#' will fail. Usually it is fine to leave the defaults in place.
#'
#' @param x An object of class `feature_select` (list) from a call
#' call to [featureSelection()].
#' @param ... Additional arguments passed to the `fitmodel` generic S3
#'   method, which performs the appropriate search algorithm depending on the
#'   object class. This is typically performed internally via the `frmla =`
#'   argument which is used to create a formula prior to being passed to one of:
#'   [glm()], `robustNaiveBayes()`, [lm()].
#' @author Kirk DeLisle & Stu Field
#' @seealso [featureSelection()], [glm()], [lm()], `robustNaiveBayes()`,
#' @seealso [modelType_glm()], [modelType_lm()], [modelType_nb()]
#' @export
fitmodel <- function(x, ...) UseMethod("fitmodel")


#' Fit Model Type: Logistic Regression
#'
#' @noRd
#' @importFrom stats glm predict
fitmodel.fs_glm <- function(x, ...) {

  # ensure response is a factor
  if ( !is.factor(x$data[, x$model.type$response ]) ) {
    x$data[, x$model.type$response] <- factor(x$data[, x$model.type$response])
  }

  args <- list(...)
  run  <- getRun(x)
  fold <- getFold(x)

  trn.rows <- x$cross.val[[run]][[fold]]$training.rows
  tst.rows <- x$cross.val[[run]][[fold]]$test.rows

  fit   <- stats::glm(args$frmla, family = "binomial",
                      data = x$data[trn.rows, ], model = FALSE)
  tst_p <- stats::predict(fit, x$data[ tst.rows, x$candidate.markers ],
                          type = "response")

  # pack all the results for return
  x$cross.val[[run]][[fold]]$model         <- stripLMC(fit)
  x$cross.val[[run]][[fold]]$fitted.values <- fit$fitted.values
  x$cross.val[[run]][[fold]]$test.predicts <- tst_p
  invisible(x)
}


#' Fit Model Type: Naive Bayes
#'
#' @noRd
fitmodel.fs_nb <- function(x, ...) {

  # ensure response is a factor
  if ( !is.factor(x$data[,x$model.type$response ]) ) {
    x$data[, x$model.type$response ] <- factor(x$data[, x$model.type$response])
  }

  args <- list(...)
  run  <- getRun(x)
  fold <- getFold(x)

  trn.rows <- x$cross.val[[run]][[fold]]$training.rows
  tst.rows <- x$cross.val[[run]][[fold]]$test.rows

  fit   <- robustNaiveBayes(args$frmla, data = x$data[trn.rows, ])
  tst_p <- predict(fit, x$data[ tst.rows, x$candidate.markers ], type = "raw")

  # pack all the results for return
  x$cross.val[[run]][[fold]]$model         <- fit
  x$cross.val[[run]][[fold]]$test.predicts <- tst_p[, fit$levels[2L]]
  invisible(x)
}


#' Fit Model Type: Linear Regression
#'
#' @importFrom stats lm predict
#' @noRd
fitmodel.fs_lm <- function(x, ...) {

  # ensure response is continuous
  if ( !is.numeric(x$data[,x$model.type$response ]) ) {
    x$data[, x$model.type$response ] <- numeric(x$data[, x$model.type$response])
  }

  args <- list(...)
  run  <- getRun(x)
  fold <- getFold(x)

  trn.rows <- x$cross.val[[run]][[fold]]$training.rows
  tst.rows <- x$cross.val[[run]][[fold]]$test.rows

  fit   <- stats::lm(args$frmla, data = x$data[ trn.rows, ], model = FALSE)
  tst_p <- stats::predict(fit, x$data[ tst.rows, x$candidate.markers ],
                          type = "response")

  # pack all the results for return
  x$cross.val[[run]][[fold]]$model         <- stripLMC(fit)
  x$cross.val[[run]][[fold]]$fitted.values <- fit$fitted.values
  x$cross.val[[run]][[fold]]$test.predicts <- tst_p
  invisible(x)
}
