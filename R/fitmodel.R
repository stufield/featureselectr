#' Model Fitting Methods of Various Forms
#'
#' Provides linear regression, Naive Bayes, and logistic regression
#'   functionality within the Feature Selection framework. The model type object
#'   requires an input object that is structured appropriately.
#'   This functionality is performed internally within the feature selection
#'   algorithm and is unlikely to require direct user calls.
#'
#' The feature selection framework provides for generic model types to be
#'   defined. This S3 method allows for model types to be fit for appropriately
#'   structured objects. The model input object *must* be structured
#'   appropriately. Note the inclusion of a `frmla` argument
#'   with the desired formula. Without this, and in the correct form, the
#'   method will fail. It is typically fine to leave the defaults in place.
#'
#' @param x A `feature_select` class object.
#'
#' @param ... Additional arguments passed to the `fitmodel` generic S3
#'   method, which performs the appropriate search algorithm depending on the
#'   object class. This is typically performed internally via the `frmla =`
#'   argument which is used to create a formula prior to being passed to one of:
#'   [stats::glm()], [stats::lm()], `.fit_nb()`.
#'
#' @author Stu Field, Kirk DeLisle
#'
#' @seealso [feature_selection()], [stats::glm()], [stats::lm()]
#' @seealso [model_type_lr()], [model_type_lm()], [model_type_nb()]
#'
#' @importFrom stats lm predict model.frame glm
#' @noRd
.fitmodel <- function(x, ...) UseMethod(".fitmodel")

# Fit Model Type: Logistic Regression
.fitmodel.fs_lr <- function(x, ...) {

  # ensure response is a factor
  if ( !is.factor(x$data[[x$model_type$response]]) ) {
    x$data[[x$model_type$response]] <- factor(x$data[[x$model_type$response]])
  }

  args <- list(...)
  run  <- get_run(x)
  fold <- get_fold(x)

  trn_rows <- x$cross_val[[run]][[fold]]$training_rows
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows

  mf    <- stats::model.frame(args$frmla, data = x$data[trn_rows, ])
  q_glm <- be_quiet(stats::glm)   # silence warnings
  fit   <- q_glm(args$frmla, data = mf, family = "binomial", model = FALSE)$result
  tst_p <- stats::predict(fit, x$data[tst_rows, x$candidate_features, drop = FALSE],
                          type = "response") |> unname()

  x$cross_val[[run]][[fold]]$test_predicts <- tst_p
  invisible(x)
}

# Fit Model Type: Naive Bayes
.fitmodel.fs_nb <- function(x, ...) {

  # ensure response is a factor
  if ( !is.factor(x$data[[x$model_type$response]]) ) {
    x$data[[x$model_type$response]] <- factor(x$data[[x$model_type$response]])
  }

  args <- list(...)
  run  <- get_run(x)
  fold <- get_fold(x)

  trn_rows <- x$cross_val[[run]][[fold]]$training_rows
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows

  mf    <- stats::model.frame(args$frmla, data = x$data[trn_rows, ])
  fit   <- .fit_nb(args$frmla, data = mf)
  tst_p <- predict(fit, x$data[tst_rows, x$candidate_features, drop = FALSE],
                   type = "raw")

  x$cross_val[[run]][[fold]]$test_predicts <- tst_p[, fit$levels[2L]]
  invisible(x)
}

# Fit Model Type: Linear Regression
.fitmodel.fs_lm <- function(x, ...) {

  # ensure response is continuous
  if ( !is.numeric(x$data[[x$model_type$response]]) ) {
    x$data[[x$model_type$response]] <- numeric(x$data[[x$model_type$response]])
  }

  args <- list(...)
  run  <- get_run(x)
  fold <- get_fold(x)

  trn_rows <- x$cross_val[[run]][[fold]]$training_rows
  tst_rows <- x$cross_val[[run]][[fold]]$test_rows

  mf  <- stats::model.frame(args$frmla, data = x$data[trn_rows, ])
  fit <- stats::lm(args$frmla, data = mf, model = FALSE)
  tst_p <- stats::predict(fit, x$data[tst_rows, x$candidate_features, drop = FALSE],
                          type = "response")

  x$cross_val[[run]][[fold]]$test_predicts <- tst_p
  invisible(x)
}
