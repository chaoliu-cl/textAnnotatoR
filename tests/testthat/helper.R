# tests/testthat/helper.R

# Helper functions for tests

library(shiny)
library(mockery)

# Create a mock Shiny session
mock_session <- function() {
  structure(
    list(
      ns = function(x) x,
      sendCustomMessage = function(...) NULL,
      sendInputMessage = function(...) NULL
    ),
    class = "ShinySession"
  )
}

# Setup test environment with temporary directory
setup_test_env <- function() {
  # Create temporary directory for tests
  test_dir <- file.path(tempdir(), "textAnnotatoR-tests")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  list(
    test_dir = test_dir
  )
}

# Cleanup test environment
cleanup_test_env <- function(env) {
  unlink(env$test_dir, recursive = TRUE)
}

# Create a mock reactive values object
mock_reactive_values <- function() {
  env <- new.env()
  env$text <- ""
  env$annotations <- data.frame(
    start = numeric(),
    end = numeric(),
    text = character(),
    code = character(),
    memo = character(),
    stringsAsFactors = FALSE
  )
  env$codes <- character()
  env$code_tree <- data.tree::Node$new("Root")
  env$code_colors <- list()
  env$memos <- list()
  env$code_descriptions <- list()
  env$history <- list(list(text = "", annotations = data.frame()))
  env$history_index <- 1
  env$current_project <- NULL
  env$project_modified <- FALSE
  env$action_history <- list()
  env$action_index <- 0
  env$merged_codes <- list()

  return(env)
}

# Create test annotations data frame
create_test_annotations <- function() {
  data.frame(
    start = c(1, 5, 10),
    end = c(3, 8, 13),
    text = c("One", "Two", "Three"),
    code = c("code1", "code2", "code3"),
    memo = c("memo1", "memo2", "memo3"),
    stringsAsFactors = FALSE
  )
}

# Create a test code hierarchy
create_test_hierarchy <- function() {
  root <- data.tree::Node$new("Root")
  theme1 <- root$AddChild("Theme1")
  theme1$type <- "theme"
  theme1$AddChild("Code1")$type <- "code"
  theme1$AddChild("Code2")$type <- "code"

  theme2 <- root$AddChild("Theme2")
  theme2$type <- "theme"
  theme2$AddChild("Code3")$type <- "code"

  return(root)
}

# Mock file operations
mock_file_ops <- function(env) {
  list(
    get_project_dir = function() env$test_dir,
    init_data_dir = function() env$test_dir,
    get_export_dir = function() file.path(env$test_dir, "exports")
  )
}
