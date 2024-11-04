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
#' call to [feature_selection()].
#' @param ... Additional arguments passed to the `fitmodel` generic S3
#'   method, which performs the appropriate search algorithm depending on the
#'   object class. This is typically performed internally via the `frmla =`
#'   argument which is used to create a formula prior to being passed to one of:
#'   [glm()], `fit_nb()`, [lm()].
#' @author Kirk DeLisle & Stu Field
#' @seealso [feature_selection()], [glm()], [lm()], [fit_nb()],
#' @seealso [model_type_glm()], [model_type_lm()], [model_type_nb()]
#' @export
fitmodel <- function(x, ...) UseMethod("fitmodel")


#' Fit Model Type: Logistic Regression
#'
#' @noRd
#' @importFrom libml stripLMC
#' @importFrom stats glm predict
fitmodel.fs_glm <- function(x, ...) {

  # ensure response is a factor
  if ( !is.factor(x$data[, x$model_type$response ]) ) {
    x$data[[x$model_type$response]] <- factor(x$data[[x$model_type$response]])
  }

  args <- list(...)
  run  <- get_run(x)
  fold <- get_fold(x)

  trn_rows <- x$cross_val[[run]][[fold]]$training_rows
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows

  fit <- stats::glm(args$frmla, family = "binomial",
                    data = x$data[trn_rows, ], model = FALSE)
  tst_p <- stats::predict(fit, x$data[tst_rows, x$candidate_markers],
                          type = "response")

  # pack all the results for return
  x$cross_val[[run]][[fold]]$model         <- stripLMC(fit)
  x$cross_val[[run]][[fold]]$fitted_values <- fit$fitted.values
  x$cross_val[[run]][[fold]]$test_predicts <- tst_p
  invisible(x)
}


#' Fit Model Type: Naive Bayes
#'
#' @importFrom libml fit_nb
#' @noRd
fitmodel.fs_nb <- function(x, ...) {

  # ensure response is a factor
  if ( !is.factor(x$data[[x$model_type$response]]) ) {
    x$data[[x$model_type$response]] <- factor(x$data[[ x$model_type$response]])
  }

  args <- list(...)
  run  <- get_run(x)
  fold <- get_fold(x)

  trn_rows <- x$cross_val[[run]][[fold]]$training_rows
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows

  fit   <- fit_nb(args$frmla, data = x$data[trn_rows, ])
  tst_p <- predict(fit, x$data[tst_rows, x$candidate_markers],
                   type = "raw")

  # pack all the results for return
  x$cross_val[[run]][[fold]]$model         <- fit
  x$cross_val[[run]][[fold]]$test_predicts <- tst_p[, fit$levels[2L]]
  invisible(x)
}


#' Fit Model Type: Linear Regression
#'
#' @importFrom stats lm predict
#' @importFrom libml stripLMC
#' @noRd
fitmodel.fs_lm <- function(x, ...) {

  # ensure response is continuous
  if ( !is.numeric(x$data[,x$model_type$response ]) ) {
    x$data[, x$model_type$response ] <- numeric(x$data[, x$model_type$response])
  }

  args <- list(...)
  run  <- get_run(x)
  fold <- get_fold(x)

  trn_rows <- x$cross_val[[run]][[fold]]$training_rows
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows

  fit <- stats::lm(args$frmla, data = x$data[trn_rows, ], model = FALSE)
  tst_p <- stats::predict(fit, x$data[ tst_rows, x$candidate_markers ],
                          type = "response")

  # pack all the results for return
  x$cross_val[[run]][[fold]]$model         <- stripLMC(fit)
  x$cross_val[[run]][[fold]]$fitted_values <- fit$fitted.values
  x$cross_val[[run]][[fold]]$test_predicts <- tst_p
  invisible(x)
}
