#' Get Wilcox Peak
#'
#' Calculate differences from the peak performance based
#'   on Wilcoxon Signed-Rank Test.
#'
#' @param x The `boxtbl` object created during the S3 plot method.
#'
#' @param type Whether forward or backward calculations should be made.
#'
#' @return A `numeric(3)` object containing:
#'   `c(index of peak,
#'      index of p = 0.05,
#'      index of p = 0.001)`.
#'
#' @importFrom stats median wilcox.test setNames
#' @noRd
get_peak_wilcox <- function(x, type = c("forward", "backward")) {

  forward <- match.arg(type) == "forward"

  if ( nrow(x) < 10L ) {
    stop(
      "Large-sample Normal approximation of Wilcoxon Signed Rank ",
      "test statistic invalid for n < 10", call. = FALSE
    )
  }

  peak_idx <- which.max(apply(x, 2, median, na.rm = TRUE)) |> unname()

  if ( (peak_idx == 1L && forward) || (peak_idx == ncol(x) && !forward) ) {
    # special case catch
    return(rep(peak_idx, 3L))
  }

  iter <- if ( forward ) {
    1L:(peak_idx - 1L)
  } else {
    (peak_idx + 1L):ncol(x)
  }
  wilcox <- lapply(iter, function(.x) {
    y1 <- if ( forward ) x[[peak_idx]] else x[[.x]]
    y2 <- if ( forward ) x[[.x]] else x[[peak_idx]]
    wilcox.test(y1, y2, exact = FALSE, correct = FALSE, paired = TRUE,
                alternative = ifelse(forward, "g", "l"))
    }) |>
    setNames(names(x)[iter])

  bool1 <- vapply(wilcox, function(x) x$p.value < 0.05, NA)
  bool2 <- vapply(wilcox, function(x) x$p.value < 0.001, NA)

  if ( forward ) {
    c(max    = peak_idx,
      p0.05  = detect_idx(bool1, isTRUE, dir = "backward"),
      p0.001 = detect_idx(bool2, isTRUE, dir = "backward")
    ) |>
    discard_zeros()
  } else {
    out <- c(p0.05  = detect_idx(bool1, isTRUE),
      p0.001 = detect_idx(bool2, isTRUE)
    ) |>
    discard_zeros()
    c(max = peak_idx, out + peak_idx)
  }
}


#' Get SE Peak
#'
#' Calculate differences from the peak performance based
#'   on Standard Errors of the mean.
#'
#' @param x The `"boxtbl"` object created during the S3 plot method.
#' @param type Whether forward or backward calculations should be made.
#' @return vector of length 3: `c(index of peak, index of -1se, index of -1.96se)`
#' @keywords internal
#' @noRd
get_peak_se <- function(x, type = c("forward", "backward")) {

  forward     <- match.arg(type) == "forward"
  cost_mean   <- apply(x, 2, mean)
  peak_idx    <- unname(which.max(cost_mean))
  box_max_est <- max(cost_mean)                             # max cost value
  se    <- sd(x[[peak_idx]]) / sqrt(length(x[[peak_idx]]))  # se max cost model
  iter  <- if ( forward ) {
    1L:max(1L, (peak_idx - 1L))   # max(): cannot have peak < 1
  } else {
    min(ncol(x), (peak_idx + 1L)):ncol(x)  # min(): cannot have peak > ncol()
  }
  bool1 <- vapply(iter, function(x) abs(box_max_est - cost_mean[x]) > se, NA)
  bool2 <- vapply(iter, function(x) abs(box_max_est - cost_mean[x]) > 1.96 * se, NA)

  if ( forward ) {
    c(max    = peak_idx,
      p0.05  = detect_idx(bool1, isTRUE, dir = "backward"),
      p0.001 = detect_idx(bool2, isTRUE, dir = "backward")
    ) |>
    discard_zeros()
  } else {
    out <- c(p0.05  = detect_idx(bool1, isTRUE),
      p0.001 = detect_idx(bool2, isTRUE)
    ) |>
    discard_zeros()
    c(max = peak_idx, out + peak_idx)
  }
}

# similar to purrr::detect_index()
detect_idx <- function(x, f, dir = c("forward", "backward"), ...) {
  dir <- match.arg(dir)
  index <- if ( dir == "forward" ) seq(x) else rev(seq(x))
  for ( i in index ) {
    if ( f(x[[i]], ...) ) {
      return(i)
    }
  }
  0L
}

discard_zeros <- function(x) {
  x[x != 0]
}
