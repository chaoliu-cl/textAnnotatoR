# tests/testthat/test-project.R
library(testthat)

test_that("project state save and load work correctly", {
  # Create test data
  test_state <- list(
    text = "Test text",
    annotations = data.frame(
      start = c(1, 5),
      end = c(4, 8),
      code = c("code1", "code2"),
      stringsAsFactors = FALSE
    ),
    codes = c("code1", "code2"),
    code_tree = Node$new("Root"),
    code_colors = c(code1 = "#FF0000", code2 = "#00FF00"),
    memos = list(),
    code_descriptions = list()
  )

  # Test saving
  temp_file <- tempfile(fileext = ".rds")

  # Mock dependencies for testing
  mockery::stub(save_project_state, "get_project_dir", dirname(temp_file))
  mockery::stub(save_project_state, "handle_error", function(expr, ...) {
    # Directly evaluate the expression
    force(expr)
  })

  mockery::stub(load_project_state, "get_project_dir", dirname(temp_file))
  mockery::stub(load_project_state, "handle_error", function(expr, ...) {
    # Directly evaluate the expression
    force(expr)
  })

  # Execute test
  save_project_state(test_state, temp_file)
  testthat::expect_true(file.exists(temp_file))

  # Test loading
  loaded_state <- load_project_state(temp_file)
  testthat::expect_equal(loaded_state$text, test_state$text)
  testthat::expect_equal(loaded_state$codes, test_state$codes)
  testthat::expect_equal(loaded_state$code_colors, test_state$code_colors)

  # Clean up
  unlink(temp_file)
})

test_that("create_plain_text_annotations formats correctly", {
  text <- "This is a test text."
  annotations <- data.frame(
    start = c(1, 10),
    end = c(4, 14),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )

  formatted_text <- create_plain_text_annotations(text, annotations)
  testthat::expect_type(formatted_text, "character")
  testthat::expect_true(grepl("\\[code1:", formatted_text))
  testthat::expect_true(grepl("\\[code2:", formatted_text))
})
