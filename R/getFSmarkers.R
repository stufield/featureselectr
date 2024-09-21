#' Get Feature Selection Markers
#'
#' Function to return the maximum, se1, and se2 markers from either
#' forward or backward selection from a `feature_select` class object.
#'
#' @param x A `feature_select` search object.
#' @return A list containing:
#' \item{max.markers}{Combination of SOMAmers that gives maximum/minimum
#'   mean cost function.}
#' \item{markers.1se.from.max}{Combination of SOMAmers that has a mean
#'   cost function that is one standard error (SE) from the
#'   maximum/minimum mean cost function.}
#' \item{markers.2se.from.max}{Combination of SOMAmers that has a mean
#'       cost function that is 1.96*SE from the maximum/minimum mean
#'       cost function.}
#' @author Stu Field
#' @examples
#' data <- splyr::sim_test_data
#' apts <- attributes(data)$sig_feats$class
#' fs <- featureSelection(data, candidate.markers = apts,
#'                        search.type = searchType_forwardModel(),
#'                        model.type = modelType_glm("class_response"),
#'                        stratified = TRUE,
#'                        cost = "AUC", runs = 2, folds = 2)
#' fs_obj <- Search(fs)
#' getFSmarkers(fs_obj)
#' @importFrom stats sd
#' @importFrom stats setNames
#' @export
getFSmarkers <- function(x) {

  if ( inherits(x, c("fs_forward_model")) ) {
     marker.index <- "cumul.markers"
  } else if ( inherits(x, "fs_backward_model") ) {
     marker.index <- "elim.markers"
  } else if ( inherits(x, "fs_forward_param") ) {
    stop("Forward Parameter Searches have been deprecated.", call. = FALSE)
  } else {
    stop("Couldn't determine `searchType` of this search object.", call. = FALSE)
  }

  max.steps <- x$search.type$max.steps      # steps
  restbl    <- x$cross.val$search.progress  # progress mean/95% CI
  csttbl    <- x$cross.val$cost.tables      # complete cost tables

  bxtbl <- lapply(1:x$search.type$max.steps, function(step) {
                  mrkr <- restbl[[marker.index]][[step]]
                  c(as.vector(csttbl[[step]][[as.character(mrkr)]]))
            }) |>
    data.frame() |>
    setNames(restbl[[marker.index]])

  max.idx     <- which.max(restbl$cost.mean)                            # idx max cost
  box.max.est <- max(restbl$cost.mean)                                  # max cost value
  se          <- sd(bxtbl[[max.idx]]) / sqrt(length(bxtbl[[max.idx]]))  # std. error max cost model

  if ( inherits(x, "fs_forward_model") ) {
    for ( se1 in max.idx:1 ) {
      if ( (box.max.est - restbl$cost.mean[se1]) < se ) next else break
    }
    for ( se2 in max.idx:1 ) {
      if ( (box.max.est - restbl$cost.mean[se2]) < se * 1.96 ) next else break
    }
    max <- x$candidate.markers[x$candidate.markers %in% restbl[[marker.index]][1:max.idx]]
    se1 <- x$candidate.markers[x$candidate.markers %in% restbl[[marker.index]][1:se1]]
    se2 <- x$candidate.markers[x$candidate.markers %in% restbl[[marker.index]][1:se2]]
  } else {
    for (se1 in max.idx:max.steps)
      if ( (box.max.est - restbl$cost.mean[se1]) < se ) next else break
    for (se2 in max.idx:max.steps)
      if ( (box.max.est - restbl$cost.mean[se2]) < se * 1.96 ) next else break
    bs.markers <- c(restbl[[marker.index]],
                    x$candidate.markers[!(x$candidate.markers %in% restbl[[marker.index]])] )
    max <- x$candidate.markers[x$candidate.markers %in% bs.markers[(max.idx + 1):length(bs.markers)]]
    se1 <- x$candidate.markers[x$candidate.markers %in% bs.markers[(se1 + 1):length(bs.markers)]]
    se2 <- x$candidate.markers[x$candidate.markers %in% bs.markers[(se2 + 1):length(bs.markers)]]
  }

  list(max.markers = max,
       markers.1se.from.max = se1,
       markers.2se.from.max = se2)
}
