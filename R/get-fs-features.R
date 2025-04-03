#' Get Feature Selection Markers
#'
#' Returns the `maximum`, `se1`, and `se2` features from
#'   a completed `feature_select` object.
#'
#' @inheritParams Search
#'
#' @return A list containing:
#'   \item{max_features}{Combination of features that gives maximum/minimum
#'     mean cost function.}
#'   \item{features_1se_from_max}{Combination of features that has a mean
#'     cost function that is one standard error (SE) from the
#'     maximum/minimum mean cost function.}
#'   \item{features_2se_from_max}{Combination of features that has a mean
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
#' @importFrom stats setNames
#' @export
get_fs_features <- function(x) UseMethod("get_fs_features")


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
#' @export
get_fs_features.fs_forward_model <- function(x) {

  check_complete(x)
  ft_index  <- "cumul_features"
  restbl    <- x$cross_val$search_progress  # progress mean/95% CI
  csttbl    <- x$cross_val$cost_tables      # complete cost tables

  bxtbl <- lapply(1:x$search_type$max_steps, function(step) {
                  mrkr <- restbl[[ft_index]][[step]]
                  c(as.vector(csttbl[[step]][[as.character(mrkr)]]))
            }) |>
    data.frame() |>
    setNames(restbl[[ft_index]])

  max_idx <- which.max(restbl$cost_mean)                      # idx max cost
  box_max_est <- max(restbl$cost_mean)                        # max cost value
  se <- sd(bxtbl[[max_idx]]) / sqrt(length(bxtbl[[max_idx]])) # std. error max cost model

  for ( se1 in max_idx:1L ) {
    if ( (box_max_est - restbl$cost_mean[se1]) < se ) next else break
  }
  for ( se2 in max_idx:1L ) {
    if ( (box_max_est - restbl$cost_mean[se2]) < se * 1.96 ) next else break
  }
  max <- x$candidate_features[x$candidate_features %in% restbl[[ft_index]][1L:max_idx]]
  se1 <- x$candidate_features[x$candidate_features %in% restbl[[ft_index]][1L:se1]]
  se2 <- x$candidate_features[x$candidate_features %in% restbl[[ft_index]][1L:se2]]

  list(max_features = max, features_1se_from_max = se1, features_2se_from_max = se2)
}

#' @noRd
#' @export
get_fs_features.fs_backward_model <- function(x) {

  check_complete(x)
  ft_index  <- "elim_features"
  max_steps <- x$search_type$max_steps      # steps
  restbl    <- x$cross_val$search_progress  # progress mean/95% CI
  csttbl    <- x$cross_val$cost_tables      # complete cost tables

  bxtbl <- lapply(1:x$search_type$max_steps, function(step) {
                  mrkr <- restbl[[ft_index]][[step]]
                  c(as.vector(csttbl[[step]][[as.character(mrkr)]]))
            }) |>
    data.frame() |>
    setNames(restbl[[ft_index]])

  max_idx <- which.max(restbl$cost_mean)                      # idx max cost
  box_max_est <- max(restbl$cost_mean)                        # max cost value
  se <- sd(bxtbl[[max_idx]]) / sqrt(length(bxtbl[[max_idx]])) # std. error max cost model

  for ( se1 in max_idx:max_steps ) {
    if ( (box_max_est - restbl$cost_mean[se1]) < se ) next else break
  }

  for ( se2 in max_idx:max_steps ) {
    if ( (box_max_est - restbl$cost_mean[se2]) < se * 1.96 ) next else break
  }

  bs_features <- c(restbl[[ft_index]],
                  x$candidate_features[!(x$candidate_features %in% restbl[[ft_index]])])

  # nolint start
  max <- x$candidate_features[x$candidate_features %in% bs_features[(max_idx + 1L):length(bs_features)]]
  se1 <- x$candidate_features[x$candidate_features %in% bs_features[(se1 + 1L):length(bs_features)]]
  se2 <- x$candidate_features[x$candidate_features %in% bs_features[(se2 + 1L):length(bs_features)]]
  # nolint end

  list(max_features = max, features_1se_from_max = se1, features_2se_from_max = se2)
}
