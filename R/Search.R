#' Feature Selection Search Algorithms
#'
#' These step-wise search methods can be used to identify a locally optimal
#' model complexity through greedy search. The methods either build up
#' (forward) or break down (backward) a model one covariate at a time based
#' upon the results of all "Runs" and "Folds" of cross-validation sets, i.e.
#' for 5 runs of 5-fold cross-validation, 25 evaluations are made to determine
#' which covariate yields the best average performance (for a given cost
#' function). See *Details* for more information on options.
#'
#' There are currently 2 search options, all of which are "greedy" algorithms:
#' \describe{
#'   \item{Forward Model Search:}{
#'     The covariate found in the first step carries
#'     through to all other steps. Likewise, the second
#'     covariate found (in combination with the first) also
#'     carries through. The results is a single model determined
#'     to be locally optimal based upon the performances across
#'     all runs and folds.}
#'   \item{Forward Parameter Search:}{
#'     This forward stepwise search types can be
#'     used to evaluate the ideal model complexity.
#'     The method builds up a model for each Run/Fold set, i.e.,
#'     for 5 runs of 5-fold cross-validation, 25 built
#'     up models will be created. Given the randomness of
#'     splitting the `Runs` and `Folds`, there is no guarantee
#'     that the built up models will be the same for
#      each Run/Fold unit. As a result, the interpretation
#'     is *what is the ideal model size*.}
#'   \item{Backward Model Search:}{
#'     The covariate removed in the first step is
#'     eliminated through all other steps. The result
#'     is a single model determined to be locally optimal
#'     based upon the performances across all runs and folds.}
#' }
#' @param x A `feature_select` class object from a call
#'   call to [feature_selection()].
#' @param num.cores Integer. How many cores to use during the search.
#'   Defaults to `1`, which does not use parallel processing. Values `> 1`
#'   only available in Linux systems.
#' @return A completed `feature_select` class object and list consisting:
#' \item{data}{The original feature data to use.}
#' \item{candidate_markers}{The list of candidate features.}
#' \item{model_type}{A list containing model type variables of the
#'   appropriate class for the desired model type.}
#' \item{search_type}{A list containing search type variables of the
#'   appropriate class for the desired search type.}
#' \item{cost}{A string of the type of cost function.}
#' \item{cost_fxn}{A list containing cost variables of the
#'   appropriate class for the desired object cost function.}
#' \item{runs}{The number of runs.}
#' \item{folds}{The number of folds.}
#' \item{do_log}{Was log-transform performed? Should be `FALSE`.}
#' \item{keep_models}{If intermediate cross-validation models are kept?}
#' \item{bootstrap}{Is bootstrapping performed?}
#' \item{stratified}{Is cross-validation stratification performed?}
#' \item{strat_column}{Which field string is used in stratification.}
#' \item{random_seed}{The random seed used}
#' \item{cross_val}{A list containing the training and test indices of the
#'   various cross validation folds.}
#' \item{search.complete}{Logical if the object has completed a search}
#' \item{call}{The original matched call.}
#' @author Kirk DeLisle, Stu Field
#' @seealso [feature_selection()]
#' @examples
#' data <- splyr::sim_adat
#'
#' # Setup response variable
#' data$class_response <- as.factor(data$class_response)
#' mt  <- model_type_glm(response = "class_response")
#' cst <- "AUC"
#' sm  <- searchType_forwardModel(max_steps = 10, display_name = "FeatureSelection Plot")
#' ft  <- head(featureselectr:::get_analytes(data))  # select candidate features
#' mcp <- feature_selection(data,
#'                          candidate_markers = ft,
#'                          model_type = mt,
#'                          search_type = sm,
#'                          cost = cst,
#'                          strat_column = "class_response",
#'                          runs = 4, folds = 3, random_seed = 99)
#'
#' FSresult <- Search(mcp)
#' FSresult
#' plot(FSresult)
#'
#' # Using parallel processing
#' # this should be ~4x faster than above
#' \dontrun{
#'   FSresult <- Search(mcp, num_cores = 4L)
#'   FSresult
#' }
#' @export
Search <- function(x, num_cores) UseMethod("Search")

#' S3 Search default method
#'
#' @noRd
#' @note Setting default fall back
#' @export
Search.default <- function(x, num_cores) {
  stop(
    "Could not find a S3 method for object of this class: ",
    value(class(x)), call. = FALSE
  )
}

#' S3 Search method for class `feature_select`
#'
#' Preforms the actual search algorithm defined within the
#' `feature_select` object.
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

  signal_rule(line_col = "green", lty = "double") |> writeLines()
  sprintf("---Starting the Feature Selection algorithm %s Search",
          x$search_method) |>
    add_style$red() |>
    writeLines()

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
