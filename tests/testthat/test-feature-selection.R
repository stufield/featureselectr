
# Setup ----
# fs_obj comes from 'helper.R'

# Testing ----
test_that("the feature select object is created correctly", {
  expect_s3_class(fs_obj, "feature_select")
  expect_named(fs_obj, c("data", "candidate_features", "model_type",
                         "search_type", "cost", "cost_fxn",
                         "runs", "folds", "random_seed", "cross_val",
                         "search_complete", "call"))
  expect_equal(fs_obj$runs, 5L)
  expect_equal(fs_obj$folds, 5L)
  expect_false(fs_obj$cross_val$stratify)
  expect_false(fs_obj$search_complete)
  expect_equal(fs_obj$cost, "AUC")
  expect_equal(fs_obj$random_seed, 101L)
  expect_equal(fs_obj$cross_val$runs, fs_obj$runs)
  expect_equal(fs_obj$cross_val$folds, fs_obj$folds)
  expect_snapshot(fs_obj)
})

test_that("the feature select update() method works correctly", {
  fs_strat <- update(fs_obj, stratify = TRUE)  # test the stratify option
  expect_s3_class(fs_strat, "feature_select")
  expect_snapshot(fs_strat)
})
