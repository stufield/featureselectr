
# Setup ----
fwd  <- search_type_forward_model("Forward", 15L)
back <- search_type_backward_model("Backward")

# Testing ----
test_that("the `search_type_*_model()` functions return correclty", {
  # defaults
  expect_equal(
    search_type_forward_model()$display_name,
    "Forward Stepwise Model Search"
  )
  expect_equal(   # defaults
    search_type_forward_model()$max_steps, 20L
  )
  expect_equal(
    search_type_backward_model()$display_name,
    "Backward Stepwise Model Search"
  )
  expect_s3_class(fwd, "fs_forward_model")
  expect_s3_class(back, "fs_backward_model")
  expect_equal(fwd$display_name, "Forward")
  expect_equal(back$display_name, "Backward")
  expect_equal(fwd$max_steps, 15L)
  expect_null(back$max_steps)
})

test_that("the `search_type_*_model()` S3 prints return correclty", {
  expect_snapshot(fwd)
  expect_snapshot(back)
})
