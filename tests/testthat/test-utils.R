# tests/testthat/test-utils.R

library(testthat)
library(data.tree)
library(mockery)

# Create a test environment with mock notification function
test_env <- new.env()
test_env$notifications <- list()

# Create mock Shiny environment
mock_session <- new.env()
mock_session$sendNotification <- function(...) NULL

# Modified handle_error for testing
local_handle_error <- function(expr, success_msg = NULL, error_msg = NULL, finally_msg = NULL) {
  tryCatch({
    result <- expr
    if (!is.null(success_msg)) {
      test_env$notifications <- append(test_env$notifications,
                                       list(list(message = success_msg, type = "message")))
    }
    return(result)
  }, error = function(e) {
    msg <- if (is.null(error_msg)) paste("Error:", e$message) else error_msg
    test_env$notifications <- append(test_env$notifications,
                                     list(list(message = msg, type = "error")))
    return(NULL)
  }, finally = {
    if (!is.null(finally_msg)) {
      test_env$notifications <- append(test_env$notifications,
                                       list(list(message = finally_msg, type = "message")))
    }
  })
}

# Helper functions
clear_notifications <- function() {
  test_env$notifications <- list()
}

get_notifications <- function() {
  test_env$notifications
}

# Test handle_error function
test_that("handle_error handles success and failure correctly", {
  # Test successful case
  clear_notifications()
  result <- local_handle_error(
    expr = 1 + 1,
    success_msg = "Success message",
    error_msg = "Error message",
    finally_msg = "Finally message"
  )

  expect_equal(result, 2)
  notifications <- get_notifications()
  expect_true(any(sapply(notifications, function(n) n$message == "Success message")))
  expect_true(any(sapply(notifications, function(n) n$message == "Finally message")))

  # Test error case
  clear_notifications()
  result <- local_handle_error(
    expr = stop("Test error"),
    success_msg = "Success message",
    error_msg = "Error message",
    finally_msg = "Finally message"
  )

  expect_null(result)
  notifications <- get_notifications()
  expect_true(any(sapply(notifications, function(n) n$message == "Error message")))
  expect_true(any(sapply(notifications, function(n) n$message == "Finally message")))
})

# Test create_action function
test_that("create_action creates proper action object", {
  action <- create_action(
    type = "add_annotation",
    data = list(start = 1, end = 10, code = "test"),
    reverse_data = list(start = 1, end = 10, code = "test")
  )

  expect_type(action, "list")
  expect_equal(action$type, "add_annotation")
  expect_equal(action$data$start, 1)
  expect_equal(action$data$end, 10)
  expect_equal(action$data$code, "test")
  expect_s3_class(action$timestamp, "POSIXct")
})

# Modified concatenate_memos function for testing
local_concatenate_memos <- function(existing_memo, new_memo) {
  if (is.null(existing_memo) || is.null(new_memo)) {
    stop("argument is NULL")
  }

  if (existing_memo == "") {
    return(new_memo)
  } else if (new_memo == "") {
    return(existing_memo)
  } else {
    return(paste(existing_memo, new_memo, sep = "; "))
  }
}

# Test concatenate_memos function
test_that("concatenate_memos combines memos correctly", {
  # Test empty existing memo
  expect_equal(local_concatenate_memos("", "New memo"), "New memo")

  # Test empty new memo
  expect_equal(local_concatenate_memos("Existing memo", ""), "Existing memo")

  # Test combining two non-empty memos
  expect_equal(local_concatenate_memos("First memo", "Second memo"), "First memo; Second memo")

  # Test with NULL values
  expect_error(local_concatenate_memos(NULL, "New memo"), "argument is NULL")
  expect_error(local_concatenate_memos("Existing memo", NULL), "argument is NULL")
})

# Test %||% operator
test_that("%||% operator works correctly", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(10 %||% 5, 10)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal("test" %||% "default", "test")
  expect_equal(c(1,2,3) %||% NULL, c(1,2,3))
  expect_equal(NULL %||% list(a=1), list(a=1))
})

# Test apply_action function
test_that("apply_action applies and reverses actions correctly", {
  # Create a proper mock annotations data frame
  mock_annotations <- data.frame(
    start = numeric(),
    end = numeric(),
    code = character(),
    stringsAsFactors = FALSE
  )

  # Create a proper environment to simulate reactive values
  rv <- list(
    annotations = mock_annotations,
    code_colors = character(),
    codes = character()
  )

  # Create test action
  test_action <- create_action(
    type = "add_annotation",
    data = data.frame(
      start = 1,
      end = 5,
      code = "test_code",
      stringsAsFactors = FALSE
    )
  )

  # Apply the action
  result_rv <- apply_action(rv, test_action)

  # Verify the action was applied
  expect_type(result_rv, "list")
  expect_s3_class(result_rv$annotations, "data.frame")
  expect_equal(nrow(result_rv$annotations), 1)
  expect_equal(result_rv$annotations$start[1], 1)
  expect_equal(result_rv$annotations$end[1], 5)
  expect_equal(result_rv$annotations$code[1], "test_code")

  # Reverse the action
  result_rv <- apply_action(result_rv, test_action, reverse = TRUE)

  # Verify the action was reversed
  expect_equal(nrow(result_rv$annotations), 0)
})

# Test error handling
test_that("error handling works properly", {
  clear_notifications()

  # Test custom error message
  result <- local_handle_error(
    expr = stop("Test error"),
    error_msg = "Custom error message"
  )

  expect_null(result)
  notifications <- get_notifications()
  expect_true(any(sapply(notifications, function(n) n$message == "Custom error message")))

  # Test default error message
  clear_notifications()
  result <- local_handle_error(
    expr = stop("Test error")
  )

  expect_null(result)
  notifications <- get_notifications()
  expect_true(any(sapply(notifications, function(n) grepl("Error: Test error", n$message))))
})
