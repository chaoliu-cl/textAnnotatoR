library(testthat)
library(shiny)
library(data.tree)
library(mockery)

# Mock Shiny session and functions
mock_session <- function() {
  session <- new.env()
  session$ns <- function(x) x
  session$sendCustomMessage <- function(type, message) NULL
  session$sendInputMessage <- function(inputId, message) NULL
  session$sendNotification <- function(...) NULL
  session$updateTextAreaInput <- function(session, inputId, ...) NULL
  class(session) <- "ShinySession"
  return(session)
}

# Create a temporary test environment
test_that("handle_error function works correctly", {
  # Mock showNotification
  mockery::stub(handle_error, "shiny::showNotification", function(...) NULL)

  # Test successful execution
  result <- handle_error(
    expr = 1 + 1,
    success_msg = "Success",
    error_msg = "Error",
    finally_msg = "Done"
  )
  expect_equal(result, 2)

  # Test error handling
  result <- handle_error(
    expr = stop("Test error"),
    error_msg = "Custom error message"
  )
  expect_null(result)
})

test_that("save_project_state works with temporary directory", {
  # Create temporary project directory
  project_dir <- file.path(tempdir(), "projects")
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)

  # Mock get_project_dir to return our temp directory
  mockery::stub(save_project_state, "get_project_dir", function() project_dir)

  # Mock handle_error to execute expression directly
  mockery::stub(save_project_state, "handle_error", function(expr, ...) expr)

  # Setup test data
  test_state <- list(
    text = "Sample text",
    annotations = data.frame(
      start = c(1),
      end = c(5),
      code = c("test"),
      stringsAsFactors = FALSE
    ),
    codes = c("test"),
    code_tree = Node$new("Root"),
    code_colors = c(test = "#FF0000"),
    memos = list(),
    code_descriptions = list()
  )

  # Test saving
  filename <- "test_project.rds"
  save_project_state(test_state, filename)

  # Verify file exists
  expect_true(file.exists(file.path(project_dir, filename)))

  # Clean up
  unlink(project_dir, recursive = TRUE)
})

test_that("load_project_state works with temporary directory", {
  # Create temporary project directory
  project_dir <- file.path(tempdir(), "projects")
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)

  # Mock get_project_dir and showNotification
  mockery::stub(load_project_state, "get_project_dir", function() project_dir)
  mockery::stub(load_project_state, "showNotification", function(...) NULL)
  mockery::stub(load_project_state, "handle_error", function(expr, ...) expr)

  # Setup test data
  test_state <- list(
    text = "Sample text",
    version = package_version("0.1.0"),
    annotations = data.frame(
      start = c(1),
      end = c(5),
      code = c("test"),
      stringsAsFactors = FALSE
    )
  )

  # Save test file
  filename <- "test_project.rds"
  saveRDS(test_state, file.path(project_dir, filename))

  # Test loading
  loaded_state <- load_project_state(filename)
  expect_equal(loaded_state$text, test_state$text)
  expect_equal(loaded_state$annotations, test_state$annotations)

  # Test loading non-existent file
  expect_null(load_project_state("nonexistent.rds"))

  # Clean up
  unlink(project_dir, recursive = TRUE)
})

test_that("save_as_html works with temporary directory", {
  # Mock update_text_display
  mockery::stub(save_as_html, "update_text_display", function(rv) "Sample HTML content")

  # Setup test data using a list instead of reactiveValues
  rv <- list(
    text = "Sample text",
    annotations = data.frame(
      start = c(1),
      end = c(5),
      code = c("test"),
      stringsAsFactors = FALSE
    ),
    code_colors = c(test = "#FF0000")
  )

  # Use temporary file
  temp_file <- tempfile(fileext = ".html")

  # Test saving
  save_as_html(temp_file, rv)
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

test_that("save_as_text works with temporary directory", {
  # Setup test data using a list instead of reactiveValues
  rv <- list(
    text = "Sample text",
    annotations = data.frame(
      start = c(1),
      end = c(5),
      code = c("test"),
      stringsAsFactors = FALSE
    )
  )

  # Use temporary file
  temp_file <- tempfile(fileext = ".txt")

  # Test saving
  save_as_text(temp_file, rv)
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

test_that("create_plain_text_annotations works correctly", {
  text <- "This is a sample text"
  annotations <- data.frame(
    start = c(1, 8),
    end = c(4, 15),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )

  result <- create_plain_text_annotations(text, annotations)

  # Test each part of the output string
  expect_match(result, "\\[code1: This\\]")
  expect_match(result, "\\[code2:\\s+a sampl\\]")  # Allow for whitespace after the colon

  # Test the complete structure with exact spacing
  expect_equal(
    result,
    "[code1: This] is[code2:  a sampl]e text"  # Note the double space after code2:
  )
})

test_that("create_new_project resets all values", {
  # Setup mock session with additional required functions
  session <- mock_session()

  # Create reactive values
  rv <- reactiveValues(
    text = "old text",
    annotations = data.frame(start = 1, end = 1, text = "a", code = "b"),
    codes = c("test"),
    code_tree = Node$new("Root"),
    code_colors = c(test = "#FF0000"),
    memos = list(test = "memo"),
    code_descriptions = list(test = "desc"),
    history = list(list(text = "old", annotations = data.frame())),
    history_index = 2,
    current_project = "test",
    project_modified = TRUE,
    action_history = list("action"),
    action_index = 1,
    merged_codes = list(a = "b")
  )

  # Mock showNotification and updateTextAreaInput
  mockery::stub(create_new_project, "showNotification", function(...) NULL)
  mockery::stub(create_new_project, "updateTextAreaInput", function(...) NULL)

  # Run in reactive environment
  shiny::isolate({
    create_new_project(rv, session)

    expect_equal(rv$text, "")
    expect_equal(nrow(rv$annotations), 0)
    expect_equal(length(rv$codes), 0)
    expect_equal(rv$code_tree$name, "Root")
    expect_equal(length(rv$code_colors), 0)
    expect_equal(length(rv$memos), 0)
    expect_equal(length(rv$code_descriptions), 0)
    expect_equal(rv$history_index, 1)
    expect_null(rv$current_project)
    expect_false(rv$project_modified)
    expect_equal(rv$action_index, 0)
    expect_equal(length(rv$merged_codes), 0)
  })
})
