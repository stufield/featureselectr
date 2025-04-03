
# Setup ----
# fs_obj comes from 'helper.R'
fs_obj_complete <- readRDS(test_path("testdata/fs-obj-complete.rds"))

# Testing ----
test_that("the `Search()` algorithm generates a completed object correctly", {
  expect_s3_class(fs_obj_complete, "feature_select")
  expect_named(fs_obj_complete, c("data", "candidate_features", "model_type",
                                  "search_type", "cost", "cost_fxn",
                                  "runs", "folds", "random_seed", "cross_val",
                                  "search_complete", "call"))
  expect_equal(names(fs_obj), names(fs_obj_complete))
  expect_equal(fs_obj$runs, fs_obj_complete$runs)
  expect_equal(fs_obj$folds, fs_obj_complete$folds)
  expect_equal(fs_obj$cross_val$stratify, fs_obj_complete$cross_val$stratify)
  expect_true(fs_obj_complete$search_complete)
  expect_equal(fs_obj$cost, fs_obj_complete$cost)
  expect_equal(fs_obj$random_seed, fs_obj_complete$random_seed)
  expect_snapshot(fs_obj_complete)
})
