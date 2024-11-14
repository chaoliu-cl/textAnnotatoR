library(testthat)
library(shiny)

# Setup test environment
test_that("test environment is properly set up", {
  expect_true(requireNamespace("testthat", quietly = TRUE))
  expect_true(requireNamespace("shiny", quietly = TRUE))
})

# Test handle_error function
test_that("handle_error works correctly", {
  # Test successful execution
  result <- handle_error(
    expr = 1 + 1,
    success_msg = "Success!",
    finally_msg = "Completed"
  )
  expect_equal(result, 2)

  # Test error handling
  result_error <- handle_error(
    expr = stop("Test error"),
    error_msg = "Custom error message",
    finally_msg = "Completed anyway"
  )
  expect_null(result_error)
})

# Test create_action function
test_that("create_action creates correct action object", {
  action <- create_action(
    type = "add_annotation",
    data = list(start = 1, end = 10, code = "test_code"),
    reverse_data = list(start = 1, end = 10, code = "test_code")
  )

  expect_type(action, "list")
  expect_equal(action$type, "add_annotation")
  expect_equal(action$data$start, 1)
  expect_equal(action$data$end, 10)
  expect_equal(action$data$code, "test_code")
  expect_true(!is.null(action$timestamp))
})

# Test apply_action function
test_that("apply_action handles annotations correctly", {
  # Create reactive values
  rv <- reactiveValues(
    annotations = data.frame(
      start = integer(),
      end = integer(),
      code = character(),
      stringsAsFactors = FALSE
    ),
    code_colors = c()
  )

  # Test adding annotation
  action <- create_action(
    type = "add_annotation",
    data = data.frame(
      start = 1,
      end = 10,
      code = "test_code",
      stringsAsFactors = FALSE
    )
  )

  # Add annotation and check results
  isolate({
    apply_action(rv, action)
    expect_equal(nrow(rv$annotations), 1)
    expect_equal(rv$annotations$start[1], 1)
    expect_equal(rv$annotations$end[1], 10)
    expect_equal(rv$annotations$code[1], "test_code")
  })

  # Remove annotation and check results
  isolate({
    apply_action(rv, action, reverse = TRUE)
    expect_equal(nrow(rv$annotations), 0)
  })
})

# Test merge_codes functionality
test_that("apply_action handles code merging correctly", {
  # Set up initial state
  initial_annotations <- data.frame(
    start = c(1, 5),
    end = c(4, 8),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )
  initial_colors <- c(code1 = "#FF0000", code2 = "#00FF00")

  # Create reactive values with initial data
  rv <- reactiveValues(
    annotations = initial_annotations,
    code_colors = initial_colors
  )

  # Create merge action with reverse data
  merge_action <- create_action(
    type = "merge_codes",
    data = list(
      old_codes = c("code1", "code2"),
      new_code = "merged_code"
    ),
    reverse_data = list(
      old_codes = c("code1", "code2"),
      new_code = "merged_code",
      old_colors = initial_colors,
      original_codes = initial_annotations$code
    )
  )

  # Test merging
  isolate({
    # Verify initial state
    expect_true(all(rv$annotations$code %in% c("code1", "code2")))
    expect_true(all(c("code1", "code2") %in% names(rv$code_colors)))
    expect_equal(rv$code_colors[["code1"]], "#FF0000")
    expect_equal(rv$code_colors[["code2"]], "#00FF00")

    # Apply merge
    apply_action(rv, merge_action)

    # Verify merged state
    expect_true(all(rv$annotations$code == "merged_code"))
    expect_true("merged_code" %in% names(rv$code_colors))
    expect_false(any(c("code1", "code2") %in% names(rv$code_colors)))
  })

  # Test unmerging
  isolate({
    # Apply reverse merge
    apply_action(rv, merge_action, reverse = TRUE)

    # Verify restored state
    expect_equal(sort(as.character(rv$annotations$code)), sort(as.character(initial_annotations$code)))
    expect_equal(sort(names(rv$code_colors)), sort(names(initial_colors)))
    expect_equal(unname(rv$code_colors["code1"]), unname(initial_colors["code1"]))
    expect_equal(unname(rv$code_colors["code2"]), unname(initial_colors["code2"]))

    # Check specific values
    expect_true(all(rv$annotations$code %in% c("code1", "code2")))
    expect_true(all(c("code1", "code2") %in% names(rv$code_colors)))
    expect_equal(rv$code_colors[["code1"]], "#FF0000")
    expect_equal(rv$code_colors[["code2"]], "#00FF00")
  })
})

# Test concatenate_memos function
test_that("concatenate_memos works correctly", {
  expect_equal(
    concatenate_memos("First memo", "Second memo"),
    "First memo; Second memo"
  )
  expect_equal(
    concatenate_memos("", "Single memo"),
    "Single memo"
  )
  expect_equal(
    concatenate_memos("Existing memo", ""),
    "Existing memo; "
  )
})

# Test %||% operator
test_that("%||% operator works correctly", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(1 %||% 2, 1)
})

# Test file operations (using tempdir)
test_that("file operations use temporary directory", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "test.txt")

  # Write some test data
  write_result <- handle_error({
    writeLines("test data", temp_file)
  })

  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})
