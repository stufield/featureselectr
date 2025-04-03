
# Testing ----
test_that("the `model_type_*()` function defaults return correclty", {
  expect_equal(model_type_lr()$response, "Response")
  expect_s3_class(model_type_lr(), "fs_lr")

  expect_equal(model_type_lm()$response, "Response")
  expect_s3_class(model_type_lm(), "fs_lm")

  expect_equal(model_type_nb()$response, "Response")
  expect_s3_class(model_type_nb(), "fs_nb")
})

test_that("the `model_type_*()` function args return correclty", {
  expect_equal(model_type_lr("foo")$response, "foo")
  expect_equal(model_type_lm("bar")$response, "bar")
  expect_equal(model_type_nb("baz")$response, "baz")
})

test_that("the `model_type_*()` S3 print methods", {
  expect_snapshot(model_type_lr("foo"))
  expect_snapshot(model_type_lm("bar"))
  expect_snapshot(model_type_nb("baz"))
})
