#' Create Model Type Function Definitions
#'
#' Declares a model type the Feature Selection framework. There are
#'   currently 3 model type options:
#'   \itemize{
#'     \item Logistic Regression Models (lr)
#'     \item Linear Regression Models (lm)
#'     \item Naive Bayes Models (nb)
#'   }
#'   Typically these functions can be called with their defaults,
#'   especially if called upon a `tr_data` class object (see \pkg{libml}).
#'
#' @name model_type
#'
#' @param response `character(1)`. The string of the column name to use as the
#'   response variable. Assumed to be a binary factor type.
#'
#' @return An object of the appropriate class according to the
#'   model type chosen, one of: `fs_lr`, `fs_nb`, or `fs_lm`.
#'
#' @author Stu Field, Kirk DeLisle
NULL


#' Model Type Logistic Regression
#'
#' @rdname model_type
#' @examples
#' # Logistic Regression
#' model_type_lr()
#'
#' @export
model_type_lr <- function(response = "Response") {
  as.list(environment()) |> add_class("fs_lr")
}

#' Model Type Naive Bayes
#'
#' @rdname model_type
#' @examples
#' # Robust Parameter Naive Bayes (default)
#' model_type_nb()
#'
#' @export
model_type_nb <- function(response = "Response") {
  as.list(environment()) |> add_class("fs_nb")
}

#' Model Type Linear Regression
#'
#' @rdname model_type
#' @examples
#' # Linear Regression
#' model_type_lm()
#'
#' @export
model_type_lm <- function(response = "Response") {
  as.list(environment()) |> add_class("fs_lm")
}

#' @noRd
#' @export
print.fs_lr <- function(x, ...) {
  signal_rule("Model: logistic regression")
  liter(x, .f = function(.x, .y) {
    signal_todo(paste0(pad(.y, 12L), value(.x)))
    })
  signal_rule()
  invisible(x)
}

#' @noRd
#' @export
print.fs_lm <- function(x, ...) {
  signal_rule("Model: linear regression")
  liter(x, .f = function(.x, .y) {
    signal_todo(paste0(pad(.y, 12L), value(.x)))
    })
  signal_rule()
  invisible(x)
}

#' @noRd
#' @export
print.fs_nb <- function(x, ...) {
  signal_rule("Model: naive Bayes")
  liter(x, .f = function(.x, .y) {
    signal_todo(paste0(pad(.y, 12L), value(.x)))
    })
  signal_rule()
  invisible(x)
}
