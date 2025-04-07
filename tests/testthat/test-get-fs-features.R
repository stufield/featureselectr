
# Setup -----
# fs_obj comes from 'helper.R'

fs_obj_complete <- readRDS(test_path("testdata/fs-obj-complete.rds"))


# Testing -----
test_that("`get_fs_features()` error catch for incomplete search objects", {
  x <- LETTERS
  expect_error(
    get_fs_features(x),   # default S3 method
    "There is no `get_fs_features()` S3 method for this class",
    fixed = TRUE
  )

  # trick into S3 but not feature_select object
  y <- structure(x, class = "fs_forward_model")
  expect_error(
    get_fs_features(y),  # caught by check_compete
    "`x` must be a `feature_select` object."
  )

  # this is for an incomplete fs_object
  expect_error(
    expect_message(
      expect_message(
        get_fs_features(fs_obj),
        "Feature selection not yet"
      ),
      "Nothing to do here. Perhaps run"
    )
  )
})

test_that("`get_fs_features()` returns the correct features", {
  x <- get_fs_features(fs_obj_complete)
  y <- list(
    features_max = c("seq.2802.68", "seq.9251.29", "seq.1942.70",
                     "seq.5751.80", "seq.9608.12", "seq.5994.84",
                     "seq.8441.53", "seq.4461.56", "seq.9297.97",
                     "seq.8797.98"),
    features_1se = c("seq.2802.68", "seq.9251.29",
                     "seq.1942.70", "seq.9608.12",
                     "seq.5994.84", "seq.8797.98"),
    features_2se = c("seq.2802.68", "seq.9251.29",
                     "seq.1942.70", "seq.9608.12",
                     "seq.5994.84")
    ) |> add_class("fs_features")
  expect_equal(x, y)
})

test_that("`get_fs_features()` S3 print method", {
  x <- get_fs_features(fs_obj_complete)
  expect_s3_class(x, "fs_features")
  expect_snapshot(x)
})
