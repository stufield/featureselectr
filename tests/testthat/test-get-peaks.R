
# Setup ----
set.seed(101)
vec <- c(1:5, seq(5, 3, by = -0.5))

# Testing ----
# Forward
test_that("`get_peak_se()` forward generates correct indices", {
  tabl0 <- lapply(seq(1, 2, length = 10), function(.x) rnorm(10, mean = .x)) |>
    data.frame() |> setNames(1:10)
  tabl1 <- lapply(vec, function(.x) rnorm(20, mean = .x)) |>
    data.frame() |> setNames(1:10)
  idx0 <- get_peak_se(tabl0)
  idx1 <- get_peak_se(tabl1)
  expect_type(idx0, "integer")
  expect_type(idx1, "integer")
  expect_equal(idx0, c(max = 9, p0.05 = 8, p0.001 = 7))
  expect_equal(idx1, c(max = 6, p0.05 = 4, p0.001 = 4))
})

# Backward
test_that("`get_peak_se()` backward generates correct indices", {
  tabl2 <- lapply(rev(vec), function(.x) rnorm(20, mean = .x)) |>
    data.frame() |> setNames(1:10)
  idx2 <- get_peak_se(tabl2, "backward")
  expect_type(idx2, "integer")
  expect_equal(idx2, c(max = 5, p0.05 = 6, p0.001 = 7))
})


# Wilcox ----
set.seed(101)
vec <- c(1:5, seq(5, 3, by = -0.5))

# Forward
test_that("`get_peak_wilcox()` forward generates correct indices", {
  tabl0 <- lapply(seq(1, 2, length = 10), function(.x) rnorm(10, mean = .x)) |>
    data.frame() |> setNames(1:10)
  tabl1 <- lapply(vec, function(.x) rnorm(20, mean = .x)) |>
    data.frame() |> setNames(1:10)
  expect_s3_class(tabl1, "data.frame")
  idx0 <- get_peak_wilcox(tabl0)
  idx1 <- get_peak_wilcox(tabl1)
  expect_type(idx0, "integer")
  expect_type(idx1, "integer")
  expect_equal(idx0, c(max = 9, p0.05 = 7))
  expect_equal(idx1, c(max = 6, p0.05 = 4, p0.001 = 4))
})

# Backward
test_that("`get_peak_wilcox()` backward generates correct indices", {
  tabl2 <- lapply(rev(vec), function(.x) rnorm(20, mean = .x)) |>
    data.frame() |> setNames(1:10)
  idx2 <- get_peak_wilcox(tabl2, "backward")
  expect_type(idx2, "integer")
  expect_equal(idx2, c(max = 5, p0.05 = 7, p0.001 = 7))
})
