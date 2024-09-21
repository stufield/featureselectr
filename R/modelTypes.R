#' Create Model Type Function Definitions
#'
#' Declares a model type the Feature Selection framework. There are
#' currently 3 model type options:
#' \itemize{
#'   \item Logistic Regression Models (glm)
#'   \item Linear Regression Models (lm)
#'   \item Naive Bayes Models (nb)
#' }
#' Typically these functions can be called with their defaults, especially if
#' called upon a data set of class `tr_data` (See Classify).
#'
#' @name modelType
#' @param response Character. The string of the column name to use as the
#' response variable. Assumed to be two-class, factor type.
#' @return An object of the appropriate class according to the 
#' model type chosen, one of: `fs_glm`, `fs_nb`, or `fs_lm`.
#' In addition, key-value pairs in a list containing:
#' \item{response}{See function arguments above.}
#' @author Kirk DeLisle, Stu Field
#' @examples
#' # Logistic Regression
#' modelType_glm()
#'
#' @importFrom SomaDataIO addClass
#' @export
modelType_glm <- function(response = "Response") {
  as.list(environment()) |> addClass("fs_glm")
}

#' Model Type Naive Bayes
#'
#' Naive Bayes Models
#'
#' @rdname modelType
#' @examples
#' # Robust Parameter Naive Bayes (default)
#' modelType_nb()
#'
#' @importFrom SomaDataIO addClass
#' @export
modelType_nb <- function(response = "Response") {
  as.list(environment()) |> addClass("fs_nb")
}

#' Model Type Linear Regression
#'
#' Linear Regression Models
#'
#' @rdname modelType
#' @examples
#' # Linear Regression
#' modelType_lm()
#'
#' @importFrom SomaDataIO addClass
#' @export
modelType_lm <- function(response = "Response") {
  as.list(environment()) |> addClass("fs_lm")
}
