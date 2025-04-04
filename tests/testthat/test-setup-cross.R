# Setup ----
x <- list()
x$runs  <- 2L
x$folds <- 5L
x$data <- mtcars
x$random_seed <- 123L

flatten <- function(x) {
  unlist(x, recursive = FALSE, use.names = FALSE) |>
    unlist(recursive = FALSE, use.names = TRUE)
}

# Testing ----
test_that("`setup_cross()` returns correct 2x5 indices for cross-val setup", {
  cv <- setup_cross.feature_select(x)$cross_val
  expect_snapshot(flatten(cv))
})

test_that("`setup_cross_strat()` returns stratified setup based on 'vs' column", {
  x$cross_val$strat_column <- "vs"
  x$cross_val$runs  <- x$runs
  x$cross_val$folds <- x$folds
  cv_strat <- setup_cross_strat.feature_select(x)$cross_val
  expect_snapshot(flatten(cv_strat))
})

test_that("`setup_cross_strat()` errors when 'strat_column' is too uniform", {
  x$data$foo <- "bar"   # all same
  x$cross_val$strat_column <- "foo"
  x$cross_val$folds <- x$folds
  expect_snapshot(setup_cross_strat.feature_select(x), error = TRUE)

  x$data$foo[1:3L] <- "baz"   # extreme bias
  expect_snapshot(setup_cross_strat.feature_select(x), error = TRUE)
})
