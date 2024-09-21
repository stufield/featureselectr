#' Robustly Fit Naive Bayes Classifier
#'
#' Computes the conditional _a_-posterior probabilities of a
#' categorical class variable given independent predictor
#' variables using the Bayes rule.
#' Parameter estimates are robustly calculated using approximations
#' of the error function for a Gaussian density, see [calcRobustGaussFit()].
#'
#' When `mad = TRUE` (median absolute deviation), non-parametric
#' calculation of Bayes' parameters are estimated,
#' namely, `mu = median(x)` and `sd = IQR(x) / 1.349`.
#' That is, `calcRobustGaussFit(..., mad = TRUE)`.
#'
#' @param x A numeric matrix, or a data frame of categorical and/or numeric
#'   variables.
#' @param y A vector indicating the true classes for each sample. Ideally a
#'   factor class object with appropriate levels.
#' @param mad Logical. Should non-parametric approximations be applied during
#'   the parameter estimation procedure. See `Details` section.
#' @param laplace positive double controlling Laplace smoothing. The default
#'   (`0`) disables Laplace smoothing.
#' @param ... Additional arguments passed to the default [robustNaiveBayes()]
#'   default method. Currently not used in the `predict` or `print` S3 methods.
#' @param keep.data Logical. Should the training data used to fit the model be
#'   included in the model object? When building thousands of models, this can
#'   become a memory issue and thus the default is `FALSE`.
#' @return `robustNaiveBayes`: A naive Bayes model with robustly fit
#'   coefficients.
#' @author Stu Field
#' @references This function was _heavily_ influenced by [e1071::naiveBayes()]
#'   See David Meyer <email: David.Meyer@R-project.org>.
#' @examples
#' head(iris)
#' # standard naiveBayes
#' m1 <- e1071::naiveBayes(Species ~ ., data = iris)  # non-robust
#' m2 <- robustNaiveBayes(iris[, -5], iris$Species)   # robust fitting
#' m3 <- robustNaiveBayes(Species ~ ., data = iris)   # with formula syntax
#' identical(sapply(m1$tables, as.numeric), sapply(m2$tables, as.numeric)) # not same
#' identical(sapply(m2$tables, as.numeric), sapply(m3$tables, as.numeric)) # same
#'
#' @importFrom stats dnorm model.extract predict sd
#' @importFrom SomaDataIO addClass
#' @noRd
robustNaiveBayes <- function(x, ...) UseMethod("robustNaiveBayes")


#' @noRd
robustNaiveBayes.default <- function(x, y, mad = FALSE, laplace = 0,
                                      keep.data = FALSE, ...) {

  Yresponse <- deparse(substitute(y))

  if ( !is.factor(y) ) {
    warning(
      "The `y` argument is a ", value(class(y)), " class vector, methods ",
      "will perform better if `y` is a factor.", call. = FALSE
    )
  }
  if ( is.null(dim(x)) ) {
    stop(
      "Argument `x` appears to be a vector.\n",
      "Are you trying to build a 1 marker model?\n",
      "Please ensure `x` is a 1 column, named, data frame or tibble.",
      call. = FALSE
    )
  }
  if ( !inherits(x, "data.frame") ) {
    x <- as.data.frame(x)
  }

  # estimation local function
  .estimate <- function(var) {
    if ( is.numeric(var) && !is.Integer(var) ) {
      do.call(
        "rbind",
        tapply(var, y, function(.x) calcRobustGaussFit(.x, mad = mad))
      )
    } else if ( is.numeric(var) && is.Integer(var) ) {
      cbind(mu    = tapply(var, y, mean, na.rm = TRUE),
            sigma = tapply(var, y, sd, na.rm = TRUE))
    } else {
      # this part doesn't make sense to me; sgf
      # See ?e1071::naiveBayes documentation
      tab <- table(y, var)
      (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
    }
  }

  # create tables
  tables <- lapply(x, .estimate)

  # fix names of dimnames
  for ( i in seq_len(length(tables)) ) {
    names(dimnames(tables[[i]])) <- c(Yresponse, colnames(x)[i])
  }

  apriori <- table(y)
  names(dimnames(apriori)) <- Yresponse

  ret <- list()
  ret$apriori <- apriori
  ret$tables  <- tables
  ret$levels  <- levels(y)
  ret$data    <- if ( keep.data ) cbind(x, Response = y) else FALSE # nolint
  ret$call    <- match.call(expand.dots = TRUE)
  addClass(ret, "robustNaiveBayes")
}


#' @noRd
robustNaiveBayes.formula <- function(formula, data, ...) {

  if ( inherits(data, "data.frame") ) {
    m       <- match.call(expand.dots = FALSE)
    m$...   <- NULL
    m[[1L]] <- as.name("model.frame")
    m       <- eval(m, parent.frame())
    Terms   <- attr(m, "terms")
    if ( any(attr(Terms, "order") > 1) ) {
      stop(
        "The `robustNaiveBayes()` function cannot currently ",
        "handle interaction terms.", call. = FALSE
      )
    }
    Response <- model.extract(m, "response")
    X        <- m[, -attr(Terms, "response"), drop = FALSE]
    return(robustNaiveBayes(X, Response, ...))
  } else {
    stop(
      "The robust naiveBayes formula interface handles data frames only.\n",
      "Please ensure the `data` argument is a `data.frame` class object.",
      call. = FALSE
    )
  }
}


#' @noRd
#' @export
print.robustNaiveBayes <- function(x, ...) {
  cat("\nRobust Naive Bayes Classifier for Discrete Predictors\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\nA-priori probabilities:\n")
  print(prop.table(x$apriori))
  cat("\nConditional densities:\n")
  l   <- length(x$levels)
  sum <- vapply(x$tables, matrix, ncol = 1, FUN.VALUE = numeric(l * 2))
  rownames(sum) <- paste0(rep(x$levels, 2), "_",
                          rep(c("mu", "sigma"), each = l))
  print(sum)
  cat("\n")
  invisible(sum)
}


#' S3 predict method for robustNaiveBayes.
#'
#' @param object A model object of class `robustNaiveBayes`.
#' @param newdata A `data.frame` with new predictors, containing at least
#'   the model covariates (but possibly more columns than the training data).
#'   Note that the column names of `newdata` are matched against the
#'   training data ones.
#' @param type If `"class"` (default), the class name with maximal
#'   posterior probability is returned for each sample, otherwise the
#'   conditional _a-posterior_ probabilities for each class are returned.
#'   Additionally, if called from within the S3 plot method, a character
#'   string determining the plot type, currently either CDF or PDF (default).
#'   Argument can be shortened and is matched.
#' @param threshold Value below which should be replaced. See `min.prob`.
#' @param min.prob Value indicating the minimum probability a prediction
#'   can take. See `threshold` argument.
#' @return `predict.robustNaiveBayes`: Depending on the `type` argument,
#'   the posterior probability of a robustly estimated naive Bayes model.
#' @examples
#' # Predictions
#' table(predict(m1, iris), iris$Species) # benchmark
#' table(predict(m2, iris), iris$Species) # approx same for Gaussian data; no outliers
#'
#' @importFrom stats dnorm
#' @export
#' @noRd
predict.robustNaiveBayes <- function(object, newdata,
                                     type = c("class", "posterior", "raw"),
                                     threshold = 1e-06,
                                     min.prob = NULL, ...) {

  type    <- match.arg(type)
  # map to either posterior or raw
  type      <- switch(type, class = "class", raw = , posterior = "posterior")
  newdata   <- as.data.frame(newdata)
  new_names <- names(newdata)
  features  <- match(names(object$tables), new_names)  # matched index col #
  features  <- new_names[features]

  if ( length(features) == 0L ) {
    stop("No common features between `model` and `newdata`.", call. = FALSE)
  }

  isnumeric <- vapply(newdata, is.numeric, FUN.VALUE = NA)
  # suppress NAs generated warning for meta data if present
  newdata <- suppressWarnings(data.matrix(newdata[, features]))
  prior   <- prop.table(c(object$apriori)) |> log()
  L <- lapply(seq_len(nrow(newdata)), function(.i) {
       ndata <- newdata[.i, ]
       likelihood <- lapply(features, function(.v) {
           nd <- ndata[[.v]]    # scalar; new data point
           if ( is.na(nd) ) {
             signal_oops(
               "Bad `newdata` in row", value(.i), "...",
               "check for NAs, non-numerics, meta data, etc."
             )
             rep.int(1L, length(prior))
           } else {
             if ( isnumeric[.v] ) {
               mu_sd <- object$tables[[.v]]                  # parameter table
               mu_sd[, 2L][ mu_sd[, 2L] == 0 ] <- threshold  # limit sd=0
               prob <- stats::dnorm(nd, mean = mu_sd[, 1L], sd = mu_sd[, 2L])
             } else {
               prob <- object$tables[[.v]][, nd]
             }
             prob[ prob == 0 ] <- threshold
             if ( !is.null(min.prob) ) {
               prob[ prob < min.prob ]     <- min.prob
               prob[ prob > 1 - min.prob ] <- 1 - min.prob
             }
             prob
           }
      }) |> data.frame() |> setNames(features)

      .checkNaiveBayesBias(likelihood)         # check excessive feature bias
      likelihood <- rowSums(log(likelihood))
      posterior  <- likelihood + prior

      if ( type != "class" ) {
        posterior <- exp(posterior) / sum(exp(posterior))
      }
      data.frame(as.list(posterior))
  }) |> dplyr::bind_rows()

  if ( type == "class" ) {
    maxprob <- apply(L, 1, which.max)
    L <- factor(object$levels[maxprob], levels = object$levels)
  }
  L
}


#' Check Naive Bayes Feature Bias
#'
#' Catch (warning) for excessive feature bias in naiveBayes likelihoods
#' during the prediction of a naive Bayes model for a single sample.
#'
#' @param likelihoods A `matrix` or `tibble` class object with the
#'   rows as the possible classes (>= 2) and the columns as the features.
#'   Likelihoods should not yet be log-transformed and entries should be as they
#'   come from [dnorm()].
#' @param max.lr The threshold maximum allowed log-likelihood ratio.
#' @return If excessive influence on likelihoods are
#'   detected a warning is triggered and the responsible feature(s) are flagged.
#' @author Stu Field
#' @examples
#' lik <- matrix(runif(6), ncol = 3)
#' rownames(lik) <- c("control", "disease")
#' colnames(lik) <- c("p1", "p2", "p3")
#' log(lik)
#' .checkNaiveBayesBias(lik)
#' .checkNaiveBayesBias(lik, max.lr = 1)   # set a low threshold
#' @noRd
.checkNaiveBayesBias <- function(likelihoods, max.lr = 1e04) {
  lr <- apply(log(likelihoods), 1L, function(.x) .x / .x[1L]) |>
    abs() |> t()
  for ( i in seq_len(nrow(lr)) ) {
    which_high <- which(lr[i, ] > max.lr)
    if ( length(which_high) > 0L ) {
      flag_feats <- colnames(likelihoods)[which_high]
      warning(
        "These features are heavily influencing the naive ",
        "Bayes likelihood: ", value(flag_feats), call. = FALSE
      )
    }
  }
}
