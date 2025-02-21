#' Get Feature Selection Markers
#'
#' Function to return the maximum, se1, and se2 markers from either
#'   forward or backward selection from a `feature_select` object.
#'
#' @param x A `feature_select` search object.
#'
#' @return A list containing:
#'   \item{max_markers}{Combination of features that gives maximum/minimum
#'     mean cost function.}
#'   \item{markers_1se_from_max}{Combination of features that has a mean
#'     cost function that is one standard error (SE) from the
#'     maximum/minimum mean cost function.}
#'   \item{markers_2se_from_max}{Combination of features that has a mean
#'     cost function that is `1.96*SE` from the maximum/minimum mean
#'     cost function.}
#'
#' @examples
#' data  <- wranglr::simdata
#' feats <- attributes(data)$sig_feats$class
#' fs <- feature_selection(data, candidate_markers = feats,
#'                         search_type = search_type_forward_model(),
#'                         model_type  = model_type_lr("class_response"),
#'                         stratified  = TRUE, cost = "AUC",
#'                         runs = 2L, folds = 2L)
#' fs_obj <- Search(fs)
#' get_markers(fs_obj)
#' @importFrom stats sd
#' @importFrom stats setNames
#' @export
get_markers <- function(x) UseMethod("get_markers")


#' @noRd
#' @export
get_markers.default <- function(x) {
  stop(
    "Could not determine `search_type` of this search object:",
    value(class(x)), call. = FALSE
  )
}

#' @noRd
#' @export
get_markers.fs_forward_param <- function(x) {
  stop("Forward Parameter Searches have been deprecated.",
       call. = FALSE)
}

#' @noRd
#' @export
get_markers.fs_forward_model <- function(x) {

  marker_index <- "cumul_markers"
  max_steps <- x$search_type$max_steps      # steps
  restbl    <- x$cross_val$search_progress  # progress mean/95% CI
  csttbl    <- x$cross_val$cost_tables      # complete cost tables

  bxtbl <- lapply(1:x$search_type$max_steps, function(step) {
                  mrkr <- restbl[[marker_index]][[step]]
                  c(as.vector(csttbl[[step]][[as.character(mrkr)]]))
            }) |>
    data.frame() |>
    setNames(restbl[[marker_index]])

  max_idx <- which.max(restbl$cost_mean)                      # idx max cost
  box_max_est <- max(restbl$cost_mean)                        # max cost value
  se <- sd(bxtbl[[max_idx]]) / sqrt(length(bxtbl[[max_idx]])) # std. error max cost model

  for ( se1 in max_idx:1 ) {
    if ( (box_max_est - restbl$cost_mean[se1]) < se ) next else break
  }
  for ( se2 in max_idx:1 ) {
    if ( (box_max_est - restbl$cost_mean[se2]) < se * 1.96 ) next else break
  }
  max <- x$candidate_markers[x$candidate_markers %in% restbl[[marker_index]][1:max_idx]]
  se1 <- x$candidate_markers[x$candidate_markers %in% restbl[[marker_index]][1:se1]]
  se2 <- x$candidate_markers[x$candidate_markers %in% restbl[[marker_index]][1:se2]]

  list(max_markers = max, markers_1se_from_max = se1, markers_2se_from_max = se2)
}

#' @noRd
#' @export
get_markers.fs_backward_model <- function(x) {

  marker_index <- "elim_markers"
  max_steps <- x$search_type$max_steps      # steps
  restbl    <- x$cross_val$search_progress  # progress mean/95% CI
  csttbl    <- x$cross_val$cost_tables      # complete cost tables

  bxtbl <- lapply(1:x$search_type$max_steps, function(step) {
                  mrkr <- restbl[[marker_index]][[step]]
                  c(as.vector(csttbl[[step]][[as.character(mrkr)]]))
            }) |>
    data.frame() |>
    setNames(restbl[[marker_index]])

  max_idx <- which.max(restbl$cost_mean)                      # idx max cost
  box_max_est <- max(restbl$cost_mean)                        # max cost value
  se <- sd(bxtbl[[max_idx]]) / sqrt(length(bxtbl[[max_idx]])) # std. error max cost model

  for (se1 in max_idx:max_steps) {
    if ( (box_max_est - restbl$cost_mean[se1]) < se ) next else break
  }

  for (se2 in max_idx:max_steps) {
    if ( (box_max_est - restbl$cost_mean[se2]) < se * 1.96 ) next else break
  }

  bs_markers <- c(restbl[[marker_index]],
                  x$candidate_markers[!(x$candidate_markers %in% restbl[[marker_index]])] )

  max <- x$candidate_markers[x$candidate_markers %in% bs_markers[(max_idx + 1):length(bs_markers)]]
  se1 <- x$candidate_markers[x$candidate_markers %in% bs_markers[(se1 + 1):length(bs_markers)]]
  se2 <- x$candidate_markers[x$candidate_markers %in% bs_markers[(se2 + 1):length(bs_markers)]]

  list(max_markers = max, markers_1se_from_max = se1, markers_2se_from_max = se2)
}
