# tests/testthat/test-text-display-functions.R

test_that("update_text_display handles empty annotations", {
  # Setup test environment
  rv <- shiny::reactiveValues(
    text = "Sample text",
    annotations = data.frame(
      start = integer(0),
      end = integer(0),
      code = character(0),
      stringsAsFactors = FALSE
    ),
    code_colors = character(0)
  )

  # Test output for text without annotations
  result <- update_text_display()
  expect_match(result, "span class='char'")
  expect_match(result, "Sample text")
})

test_that("update_text_display formats annotations correctly", {
  # Setup test environment with annotations
  rv <- shiny::reactiveValues(
    text = "Sample text for testing",
    annotations = data.frame(
      start = c(1, 8),
      end = c(6, 12),
      code = c("Code1", "Code2"),
      stringsAsFactors = FALSE
    ),
    code_colors = c(Code1 = "#FF0000", Code2 = "#00FF00")
  )

  result <- update_text_display()

  # Check for code display spans
  expect_match(result, "class='code-display'")
  expect_match(result, "background-color: #FF0000")
  expect_match(result, "background-color: #00FF00")

  # Check for code labels
  expect_match(result, "\\[Code1\\]")
  expect_match(result, "\\[Code2\\]")
})

test_that("update_text_display handles overlapping annotations", {
  # Setup test environment with overlapping annotations
  rv <- shiny::reactiveValues(
    text = "Sample text",
    annotations = data.frame(
      start = c(1, 3),
      end = c(6, 8),
      code = c("Code1", "Code2"),
      stringsAsFactors = FALSE
    ),
    code_colors = c(Code1 = "#FF0000", Code2 = "#00FF00")
  )

  result <- update_text_display()

  # Check that both annotations are present
  expect_match(result, "\\[Code1\\]")
  expect_match(result, "\\[Code2\\]")
})

test_that("create_plain_text_annotations handles basic case", {
  # Setup test environment
  rv <- shiny::reactiveValues(
    text = "Sample text for testing",
    annotations = data.frame(
      start = c(1, 8),
      end = c(6, 12),
      code = c("Code1", "Code2"),
      stringsAsFactors = FALSE
    )
  )

  result <- create_plain_text_annotations()

  # Check format
  expect_match(result, "\\[Code1: Sampl\\]")
  expect_match(result, "\\[Code2: text\\]")
})

test_that("create_plain_text_annotations handles no annotations", {
  # Setup test environment with no annotations
  rv <- shiny::reactiveValues(
    text = "Sample text",
    annotations = data.frame(
      start = integer(0),
      end = integer(0),
      code = character(0),
      stringsAsFactors = FALSE
    )
  )

  result <- create_plain_text_annotations()

  # Should return original text unchanged
  expect_equal(result, "Sample text")
})

test_that("create_plain_text_annotations handles adjacent annotations", {
  # Setup test environment with adjacent annotations
  rv <- shiny::reactiveValues(
    text = "Sample text test",
    annotations = data.frame(
      start = c(1, 7),
      end = c(6, 11),
      code = c("Code1", "Code2"),
      stringsAsFactors = FALSE
    )
  )

  result <- create_plain_text_annotations()

  # Check that adjacent annotations are properly formatted
  expect_match(result, "\\[Code1: Sampl\\]\\[Code2: text\\]")
})

# Setup and teardown helpers

# Create a temporary testing environment
create_test_env <- function() {
  list(
    rv = shiny::reactiveValues(
      text = "Sample text for testing",
      annotations = data.frame(
        start = integer(0),
        end = integer(0),
        code = character(0),
        stringsAsFactors = FALSE
      ),
      codes = character(0),
      code_colors = character(0),
      code_tree = Node$new("Root")
    ),
    temp_dir = withr::local_tempdir()
  )
}

# Clean up testing environment
cleanup_test_env <- function(env) {
  unlink(env$temp_dir, recursive = TRUE)
}
