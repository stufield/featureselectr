
.fit_nb <- function(x, ...) UseMethod(".fit_nb")

.fit_nb.default <- function(x, y, mad = FALSE, laplace = 0,
                            keep.data = FALSE, ...) {

  response <- attr(x, "response_var") %||% "y"

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

  if ( response %in% names(x) ) {
    stop("The variable ", value(response), " is included in the predictors.\n",
         "It is unlikely this was intentional!", call. = FALSE)
  }

  # estimation local function
  .estimate <- function(var) {
    if ( is.numeric(var) && !is_int_vec(var) ) {
      do.call(
        "rbind",
        tapply(var, y, function(.x) fit_gauss(.x, mad = mad))
      )
    } else if ( is.numeric(var) && is_int_vec(var) ) {
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
    names(dimnames(tables[[i]])) <- c(response, colnames(x)[i])
  }

  apriori <- table(y)
  names(dimnames(apriori)) <- response

  ret <- list()
  ret$apriori <- apriori
  ret$tables  <- tables
  ret$levels  <- levels(y)
  ret$response <- response
  resp_df     <- setNames(data.frame(y), response)
  ret$data    <- if ( keep.data ) cbind(x, resp_df) else FALSE # nolint
  ret$call    <- list(...)$orig_call %||% match.call(expand.dots = FALSE)
  add_class(ret, "fs_bayes")
}

.fit_nb.formula <- function(formula, data, ...) {
  if ( !inherits(data, "data.frame") ) {
    stop(
      "The robust naiveBayes formula interface handles data frames only.\n",
      "Please ensure the `data` argument is a `data.frame` class object.",
      call. = FALSE)
  }
  response <- as.character(formula[[2L]])
  X <- stats::model.frame(formula, data)
  idx <- which(names(X) %in% response)   # remove response
  call <- list(...)$orig_call %||% match.call(expand.dots = TRUE)
  .fit_nb(
    structure(X[, -idx, drop = FALSE], response_var = response), # add for downstream
    X[[response]],
    orig_call = call,
    ...
  )
}


#' @noRd
#' @importFrom stats dnorm
#' @export
predict.fs_bayes <- function(object, newdata,
                             type = c("raw", "class", "posterior"),
                             threshold = NULL, ...) {

  type <- match.arg(type)
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
  stopifnot(
    "All features must be numeric." = all(isnumeric[features])
  )
  prior <- log(prop.table(c(object$apriori)))
  pars  <- object$tables[features] |> # ensure order
    do.call(what = rbind)
  levs  <- object$levels
  L     <- length(levs)

  LL <- apply(newdata[, features, drop = FALSE], 1, function(row) {
    mu_vec <- pars[, "mu"]
    sd_vec <- pars[, "sigma"]
    sd_vec[sd_vec == 0] <- 1e-06  # limit sd=0 (??? sgf???)
    sample_vec <- rep(row, each = L)
    probs <- dnorm(sample_vec, mean = mu_vec, sd = sd_vec)

    if ( !is.null(threshold) ) {
      stopifnot(
        "`threshold` must be a scalar double." = is_dbl(threshold)
      )
      probs[probs < threshold] <- threshold
      probs[probs > 1 - threshold] <- 1 - threshold
    }

    likelihood <- matrix(probs, nrow = L, dimnames = list(levs, features))

    # check excessive feature bias
    if ( !is.null(flag <- .check_nb_bias(likelihood)) ) {
      warning(
        "Features heavily influencing the ",
        "Bayes likelihood: ", value(flag), call. = FALSE
      )
    }

    likelihood <- rowSums(log(likelihood))
    posterior  <- likelihood + prior

    if ( type != "class" ) {
      posterior <- exp(posterior) / sum(exp(posterior))
    }
    data.frame(as.list(posterior))
  }) |> dplyr::bind_rows()

  if ( type == "class" ) {
    maxprob <- apply(LL, 1, which.max)
    LL <- factor(object$levels[maxprob], levels = levs)
  }
  LL
}

.check_nb_bias <- function(likelihoods, max_lr = 1e04) {
  lr <- apply(log(likelihoods), 1L, function(.x) .x / .x[1L]) |>
    abs() |> t()
  if ( any(lr > max_lr) ) {
    names(keep_it(colSums(lr >= max_lr) > 0, function(x) x > 0))
  } else {
    invisible()
  }
}
