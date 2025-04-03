#' Feature Selection Search Algorithms
#'
#' These step-wise search methods can be used to identify a locally optimal
#'   model complexity through greedy search. The methods either build up
#'   (forward) or break down (backward) a model one covariate at a time based
#'   upon the results of all "Runs" and "Folds" of cross-validation sets, i.e.
#'   for 5 runs of 5-fold cross-validation, 25 evaluations are made to determine
#'   which covariate yields the best average performance (for a given cost
#'   function). See *Details* for more information on options.
#'
#' There are currently 2 search options, all of which are "greedy" algorithms:
#'
#' \describe{
#'   \item{Forward Model Search:}{
#'     The covariate found in the first step carries
#'     through to all other steps. Likewise, the second
#'     covariate found (in combination with the first) also
#'     carries through. The results is a single model determined
#'     to be locally optimal based upon the performances across
#'     all runs and folds.}
#'   \item{Backward Model Search:}{
#'     The covariate removed in the first step is
#'     eliminated through all other steps. The result
#'     is a single model determined to be locally optimal
#'     based upon the performances across all runs and folds.}
#' }
#'
#' @param x A `feature_select` class object from a call
#'   call to [feature_selection()].
#'
#' @param num_cores `integer(1)`. How many cores to use during the search.
#'   Defaults to `1L`, which does not use parallel processing. Values `> 1`
#'   only available in Linux systems.
#'
#' @inherit feature_selection return
#'
#' @author Stu Field, Kirk DeLisle
#' @seealso [feature_selection()]
#'
#' @section Silence notches:
#'   For the `plot()` routine, `Notch went outside hinges` message
#'   is often triggered by \pkg{ggplot2}. This can be silenced by
#'   setting global options:
#'
#'   ```options(rlib_message_verbosity = "quiet")```
#'
#' @examples
#' data <- wranglr::simdata
#'
#' # Setup response variable
#' data$class_response <- as.factor(data$class_response)
#' mt  <- model_type_lr("class_response")
#' sm  <- search_type_forward_model("Feature Selection Algorithm", 10L)
#' ft  <- head(helpr:::get_analytes(data))  # select candidate features
#' mcp <- feature_selection(data,
#'                          candidate_features = ft,
#'                          model_type = mt,
#'                          search_type = sm,
#'                          cost = "AUC",
#'                          runs = 4L, folds = 3L,
#'                          random_seed = 101L)
#'
#' fs <- Search(mcp)
#' fs
#'
#' plot(fs)
#'
#' # Using parallel processing:
#' #   should be ~4x faster than above
#' \dontrun{
#'   fs <- Search(mcp, num_cores = 4L)
#' }
#' @export
Search <- function(x, num_cores) UseMethod("Search")


#' S3 Search default method
#'
#' @noRd
#' @export
Search.default <- function(x, num_cores) {
  stop(
    "Could not find a S3 method for object of this class: ",
    value(class(x)), call. = FALSE
  )
}

#' S3 Search method for class `feature_select`
#'
#' @noRd
#' @export
Search.feature_select <- function(x, num_cores = 1L) {

  check_feature_select(x)

  if ( x$search_complete ) {
    stop(
      "This object has already performed a search with ",
      "these data, candidates, and search parameters.",
      call. = FALSE
    )
  }

  signal_info(
    add_style$blue("Starting the Feature Selection algorithm ...") |>
      add_style$bold()
  )

  num_cores <- parallel_setup(num_cores)

  if ( num_cores > x$runs ) {
    signal_oops(
      "You have set more cores than runs ... this is likely unintended."
    )
  }

  # Run the search
  # NextMethod() finds next class Search method
  x_res <- NextMethod("Search", num_cores = num_cores)
  x_res$search_complete <- TRUE
  x_res
}
