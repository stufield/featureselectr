#' Get Feature Selection Markers
#'
#' Returns the `maximum`, `se1`, and `se2` features from
#'   a completed `feature_select` object.
#'
#' @inheritParams Search
#'
#' @return A list containing:
#'   \item{features_max}{Combination of features that gives maximum/minimum
#'     mean cost function.}
#'   \item{features_1se}{Combination of features that has a mean
#'     cost function that is one standard error (SE) from the
#'     maximum/minimum mean cost function.}
#'   \item{features_2se}{Combination of features that has a mean
#'     cost function that is `1.96*SE` from the maximum/minimum mean
#'     cost function.}
#'
#' @examples
#' data  <- wranglr::simdata
#' feats <- attr(data, "sig_feats")$class
#' fs <- feature_selection(data, candidate_features = feats,
#'                         search_type = search_type_forward_model(),
#'                         model_type  = model_type_lr("class_response"),
#'                         runs = 2L, folds = 2L)
#' fs_obj <- Search(fs)
#' get_fs_features(fs_obj)
#' @importFrom stats sd
#' @export
get_fs_features <- function(x) UseMethod("get_fs_features")

#' @noRd
#' @export
print.fs_features <- function(x, ...) {
  L <- lengths(x)
  signal_rule("Features", line_col = "green", lty = "double")
  liter(L, .f = function(.x, .y) signal_todo(pad(.y, 13L), value(.x)))
  cat("\n")
  liter(x, .f = function(.x, .y) {
    signal_rule(.y, line_col = "blue")
    writeLines(value(.x))
  })
  invisible(x)
}

#' @noRd
#' @export
get_fs_features.default <- function(x) {
  stop(
    "There is no `get_fs_features()` S3 method for this class: ",
    value(class(x)), call. = FALSE
  )
}

#' @noRd
#' @export
get_fs_features.fs_forward_param <- function(x) {
  stop("Forward Parameter Searches have been deprecated.",
       call. = FALSE)
}

#' @noRd
#' @importFrom utils head
#' @export
get_fs_features.fs_forward_model <- function(x) {

  check_complete(x)
  restbl <- x$cross_val$search_progress  # progress mean/95% CI
  csttbl <- x$cross_val$cost_tables      # complete cost tables

  bxtbl <- liter(restbl$cumul_features, csttbl, function(ft, step) {
                 as.vector(step[[ft]])
           }) |> data.frame()

  max_idx <- which.max(restbl$cost_mean)         # idx max cost
  box_max_est <- max(restbl$cost_mean)           # max cost value
  se <- sd(bxtbl[[max_idx]]) / sqrt(nrow(bxtbl)) # std. error max cost model

  restbl$se1 <- box_max_est - restbl$cost_mean < se
  restbl$se2 <- box_max_est - restbl$cost_mean < (se * 1.96)

  max  <- head(restbl$cumul_features, max_idx)
  idx1 <- which(restbl$se1)[1L] - 1L
  idx2 <- which(restbl$se2)[1L] - 1L

  se1  <- head(restbl$cumul_features, idx1)
  se2  <- head(restbl$cumul_features, idx2)

  structure(
    list(features_max = intersect(x$candidate_features, max),  # reorders
         features_1se = intersect(x$candidate_features, se1),
         features_2se = intersect(x$candidate_features, se2)),
    class = c("fs_features", "list")
  )
}

#' @noRd
#' @importFrom utils tail
#' @export
get_fs_features.fs_backward_model <- function(x) {

  check_complete(x)
  restbl <- x$cross_val$search_progress  # progress mean/95% CI
  csttbl <- x$cross_val$cost_tables      # complete cost tables

  bxtbl <- liter(restbl$elim_features, csttbl, function(ft, step) {
                 as.vector(step[[ft]])
           }) |> data.frame()

  max_idx <- which.max(restbl$cost_mean)         # idx max cost
  box_max_est <- max(restbl$cost_mean)           # max cost value
  se <- sd(bxtbl[[max_idx]]) / sqrt(nrow(bxtbl)) # std. error max cost model

  restbl$se1 <- box_max_est - restbl$cost_mean < se
  restbl$se2 <- box_max_est - restbl$cost_mean < (se * 1.96)

  max  <- tail(restbl$elim_features, -max_idx)
  idx1 <- which(!restbl$se1)[1L]
  idx2 <- which(!restbl$se2)[1L]
  se1  <- tail(restbl$elim_features, -idx1)
  se2  <- tail(restbl$elim_features, -idx2)

  structure(
    list(features_max = intersect(x$candidate_features, max),  # reorders
         features_1se = intersect(x$candidate_features, se1),
         features_2se = intersect(x$candidate_features, se2)),
    class = c("fs_features", "list")
  )
}
