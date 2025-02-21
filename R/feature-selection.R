#' Feature Selection Object Declaration
#'
#' Declares and generates a `feature_selection` class object within the
#'   Feature Selection framework. This object acts as the holder of data
#'   (bootstrapped or cross-validation folds), model type, search type,
#'   cost function, and an underlying data structure for use by other functions.
#'
#' @param data A `data.frame` containing features and clinical
#'   data suitable for modeling.
#'
#' @param candidate_markers `character(n)`. List of candidate markers,
#'   i.e. columns names, from the data object.
#'
#' @param model_type An instantiated `model_type` object, generated via a call
#'   to one of the [model_type()] functions.
#'
#' @param search_type An instantiated `search_type` object, generated via a
#'   call to one of the [search_type()] functions.
#'
#' @param cost `character(1)`. A string to be used in defining the cost
#'   function. One of:
#'   \describe{
#'     \item{`AUC`}{Area Under the Curve}
#'     \item{`MSE`}{Mean-Squared Error}
#'     \item{`CCC`}{Concordance Correlation Coefficient}
#'     \item{`R2`}{R-squared - regression models}
#'     \item{`sens` or `spec`}{Sensitivity + Specificity}
#'   }
#'
#' @param runs `integer(1)`. How many runs to perform.
#'
#' @param folds `integer(1)`. How many fold cross-validation to perform.
#'
#' @param keep_models `logical(1)`. Should model objects be retained in the
#'   cross validation sub-list? This eats up memory (`default = FALSE`).
#'
#' @param bootstrap `logical(1)`. Should data be bootstrapped rather
#'   than set up in cross-validation folds? The result is multiple runs (defined
#'   by runs) with 1 Fold each. The full dataset will be sampled with replacement
#'   to generate a training set of equivalent size. The samples not chosen during
#'   sampling make up the test set.
#'
#' @param stratified `logical(1)`. Should cross-validation or bootstrap
#'   folds be stratified based upon the stratification column specified in
#'   `strat_column`?
#'
#' @param strat_column `character(1)`. Which column to use for stratification of
#'   cross-validation or bootstrap folds. If `NULL` (default), column name
#'   `"Response"` will be used and thus must be present.
#'
#' @param random_seed `integer(1)`. Used to control the random number generator for
#'   reproducibility.
#'
#' @param x A `feature_select` class object.
#'
#' @return A `"feature_select"` class object and list containing:
#'   \item{data}{The original feature data to use.}
#'   \item{candidate_markers}{The list of candidate features.}
#'   \item{model_type}{A list containing model type variables of the
#'     appropriate class for the desired model type.}
#'   \item{search_type}{A list containing search type variables of the
#'     appropriate class for the desired search type.}
#'   \item{cost}{A string of the type of cost function.}
#'   \item{cost_fxn}{A list containing cost variables of the
#'     appropriate class for the desired object cost function.}
#'   \item{runs}{The number of runs.}
#'   \item{folds}{The number of folds.}
#'   \item{keep_models}{If intermediate cross-validation models are kept?}
#'   \item{bootstrap}{Is bootstrapping performed?}
#'   \item{stratified}{Is cross-validation stratification performed?}
#'   \item{strat_column}{Which field string is used in stratification.}
#'   \item{random_seed}{The random seed used}
#'   \item{cross_val}{A list containing the training and test indices of the
#'     various cross validation folds.}
#'   \item{search_complete}{Logical if the object has completed a search}
#'   \item{call}{The original matched call.}
#'
#' @author Kirk DeLisle, Stu Field
#' @seealso [Search()]
#'
#' @references Hastie, Tibshirani, and Friedman.
#'   Elements of Statistical Learning: Data Mining, Inference, and Prediction.
#'   *2nd Ed*. Springer. 2009.
#'
#' @examples
#' # Simulated Test Data
#' data <- wranglr::simdata
#'
#' # Setup response variable
#' data$class_response <- factor(data$class_response)
#'
#' mt <- model_type_lr(response = "class_response")
#' cf <- "sens"
#' sm <- search_type_forward_model(15L, display_name = "FeatureSelection Plot")
#' ft <- helpr:::get_analytes(data)   # select candidate features
#' fs <- feature_selection(data, candidate_markers = ft,
#'                         model_type = mt, search_type = sm, cost = cf,
#'                         strat_column = "class_response", runs = 5L, folds = 5L)
#' # S3 Print method
#' fs
#'
#' # Using the S3 Update method to modify existing FS object:
#' # change model type, cost function, and random seed
#' fs2 <- update(fs, model_type = model_type_nb(),
#'               cost = "AUC", random_seed = 99L)
#' fs2
#'
#' # change number of runs & folds
#' # requires re-calculation of cross-validation parameters
#' fs3 <- update(fs, runs = 20L, folds = 10L)
#' fs3
#'
#' @export
feature_selection <- function(data, candidate_markers, model_type,
                              search_type, runs = 1L, folds = 1L,
                              cost = c("AUC", "R2", "CCC", "MSE", "sens", "spec"),
                              keep_models = FALSE, bootstrap = FALSE,
                              stratified = FALSE, strat_column = NULL,
                              random_seed = 101L) {

  # logic to ensure compatibility among model_type,
  # search_type, and cost selections should go here;
  # this could get messy; need to think about how best to accomplish

  if ( !inherits(data, "data.frame") ) {
    stop(
      "Wrong data type for `data =` argument.\n",
      "Must inherit from a `data.frame` class object",
      call. = FALSE
    )
  }

  if ( inherits(data, "tbl_df") ) {  # if data is a tbl_df, recast as df
    data <- data.frame(data)
  }

  cost_str <- match.arg(cost)
  cost_fxn <- switch(cost_str,
                     sens = cost_sens_spec(),
                     spec = cost_sens_spec(),
                     AUC  = cost_auc(),
                     CCC  = cost_ccc(),
                     R2   = cost_rsq(),
                     MSE  = cost_mse())

  # start the list
  fsret <- list(data = data,
                candidate_markers = candidate_markers,
                model_type = model_type,
                search_type = search_type,
                cost = cost_str,
                cost_fxn = cost_fxn,
                runs = runs,
                folds = folds,
                keep_models = keep_models,
                random_seed = random_seed,
                cross_val = list())

  if ( is.null(strat_column) ) {
    fsret$strat_column <- model_type$response
  } else {
    fsret$strat_column <- strat_column
  }

  fsret$search_type$max_steps <- min(length(candidate_markers),
                                     search_type$max_steps)

  class_hierarchy <- setdiff(unique(c("feature_select",
                                      class(model_type),
                                      class(search_type),
                                      class(cost_fxn))),
                             "list")
  fsret <- add_class(fsret, class_hierarchy)

  # build the cross-validation folds/bootstrap sets here so
  # that they are always consistent
  # there should be n-repeats or runs of k-fold cross validation
  # runs and fold_cross_val parameters
  fsret$cross_val$runs       <- runs
  fsret$cross_val$folds      <- folds
  fsret$cross_val$stratified <- stratified

  if ( stratified ) {
    fsret <- setup_cross_strat(fsret)
  } else {
    fsret <- setup_cross(fsret)
  }

  fsret$cross_val$current_run  <- 0
  fsret$cross_val$current_fold <- 0
  fsret$search_complete        <- FALSE
  fsret$search_method          <- "Model"
  fsret$call <- match.call(expand.dots = TRUE)
  fsret
}


#' S3 print method for class `feature_select`
#'
#' @rdname feature_selection
#' @export
print.feature_select <- function(x, ...) {

  signal_rule("Feature Selection Object", lty = "double", line_col = "magenta")
  signal_rule("Dataset Info", line_col = "blue")
  key <- c(
    "Rows",
    "Columns",
    "FeatureData"
  ) |> pad(25)

  value <- c(
    length(row.names(x$data)),
    length(names(x$data)),
    length(setdiff(x$candidate_markers, x$response))
  )

  liter(key, value, function(.x, .y) {
    writeLines(paste(add_color(symbl$bullet, "red"), .x, value(.y)))
  })

  signal_rule("Search Optimization Info", line_col = "red")

  key2 <- c(
    "No. Candidates",
    "Response Field",
    "Cross Validation Runs",
    "Cross Validation Folds",
    "Stratified Folds",
    "Model Type",
    "Search Type",
    "Search Method",
    "Cost Function",
    "Random Seed",
    "Keep Models",
    "Display Name",
    "Search Complete"
  ) |> pad(25)

  value2 <- c(
    length(x$candidate_markers),
    x$model_type$response,
    x$runs,
    x$folds,
    x$cross_val$stratified,
    class(x$model_type)[1L],
    class(x$search_type)[1L],
    x$search_method,
    x$cost,
    x$random_seed,
    x$keep_models,
    x$search_type$display_name,
    x$search_complete
  )

  liter(key2, value2, function(.x, .y) {
    writeLines(paste(add_color(symbl$bullet, "red"), .x, value(.y)))
  })

  signal_rule(lty = "double", line_col = "green")
  invisible(x)
}


#' @describeIn feature_selection
#'  Check if a valid `feature_select` class object.
#'
#' @export
is_feature_select <- function(x) {
  class <- inherits(x, "feature_select")
  nms   <- all(c("data",
                 "candidate_markers",
                 "model_type",
                 "cross_val",
                 "cost",
                 "call",
                 "keep_models",
                 "search_type",
                 "cost_fxn",
                 "runs",
                 "folds") %in% names(x))
  (class && nms)
}


#' The S3 update method allows for modification of existing
#' `feature_select` objects on-the-fly.
#'
#' @rdname feature_selection
#'
#' @param object An object of class `feature_select`.
#'
#' @param ... Arguments declared for update in `argument = value` format.
#'   Non-declared arguments from the *original* call are preserved.
#'
#' @export
update.feature_select <- function(object, ...) {

  if ( object$search_complete ) {
    over_write <- readline(
      paste("This `feature_select` object has already been completed,",
            "do you wish to over-write? [y/n]: ")
    )
    if ( over_write %in% c("n", "N", "c", "C") ) {
      stop(
        "\nUpdate feature selection object stopped at user's request.\n",
        call. = FALSE
      )
    }
  }

  call_update <- match.call(expand.dots = TRUE)[-1L]
  .call <- object$call

  # if not setting new seed, use old one
  if ( !"random_seed" %in% names(call_update) ) {
    .call$random_seed <- object$random_seed
  }
  for ( arg in grep("^object$", names(call_update), invert = TRUE, value = TRUE) ) {
    .call[[arg]] <- call_update[[arg]]
  }
  eval.parent(.call)
}
