# tests/testthat/test-error-handling.R

test_that("null coalescing operator works correctly", {
  a <- NULL
  b <- 5

  expect_equal(a %||% b, 5)
  expect_equal(b %||% a, 5)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(list() %||% NULL, list())
})

test_that("handle_error manages errors appropriately", {
  # Test successful execution
  result <- handle_error(
    expr = { 2 + 2 },
    success_msg = "Success"
  )
  expect_equal(result, 4)

  # Test error handling
  result <- handle_error(
    expr = { stop("Error") },
    error_msg = "Failed"
  )
  expect_null(result)
})
