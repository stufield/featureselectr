#' Strip Linear Model Components
#'
#' Strips linear model (`"lm"` or `"glm"`) object components down
#' the bare essentials to reduce size. Used typically
#' when building thousands of models, e.g. during cross-validation
#' to manage memory.
#'
#' @param x A model object of class `"lm"` or `"glm"`.
#' @return A stripped down (size) `"lm"` or `"glm"` object.
#'   For `"lm"` class, removes or sets -> 0 these elements:
#'     - `model`
#'     - `fitted`
#'     - `assign`
#'     - `effects`
#'     - `xlevels`
#'   For `"glm"` class, removes or sets to `0` these elements:
#'     - `linear.predictors`
#'     - `terms`
#'     - `formula`
#'     - `family` (`family`, `link`, `linkfun`, and `linkinv` remain)
#' @author Stu Field
#' @seealso [glm()], [lm()], [summary()], [residuals()]
#' @examples
#' fat_lm <- function() {
#'   junk <- runif(1e5)
#'   stats::lm(vs ~ ., data = mtcars)
#' }
#' fat_glm <- function() {
#'   junk <- runif(1e5)   # excessive junk in envir
#'   stats::glm(vs ~ wt + disp, data = mtcars, family = "binomial")
#' }
#'
#' # LM
#' lobstr::obj_size(fat_lm())
#' lobstr::obj_size(stripLMC(fat_lm()))
#'
#' # GLM
#' lobstr::obj_size(fat_glm())
#' lobstr::obj_size(stripLMC(fat_glm()))
#' @noRd
stripLMC <- function(x) UseMethod("stripLMC")

#' @noRd
stripLMC.default <- function(x) {
  stop(
    "Only valid models for `stripLMC()` are {value('lm')} or ",
    value("glm"), ". You passed: ", value(class(x)),
    call. = FALSE
  )
}

#' @noRd
stripLMC.lm <- function(x) {
  x$y <- numeric(0)
  x$x <- matrix(0)
  x$model <- data.frame()
  x$fitted <- numeric(0)
  rownames(x$model) <- NULL
  x$assign  <- numeric(0)
  x$effects <- numeric(0)
  x$xlevels <- list()
  x$call <- call("dummy_call")
  environment(x$terms) <- baseenv()
  x
}

#' @noRd
stripLMC.glm <- function(x) {
  x$model <- data.frame()
  x$effects <- numeric(0)
  x$fitted  <- numeric(0)
  x$linear.predictors <- numeric(0)
  x$weights <- numeric()
  x$data <- data.frame()
  x$family$aic <- numeric(0)
  x$family$validmu  <- numeric(0)
  x$family$simulate <- numeric(0)
  x$call <- call("dummy_call")
  environment(x$family$variance)   <- baseenv()
  environment(x$family$dev.resids) <- baseenv()
  environment(x$terms)   <- baseenv()
  environment(x$formula) <- baseenv()
  x
}
