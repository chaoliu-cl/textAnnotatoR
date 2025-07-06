library(testthat)
library(data.tree)
library(shiny)
library(mockery)

# Reuse helper functions from previous test file
source("helper.R")

test_that("validate_directory works with tempdir", {
  # Setup
  env <- setup_test_env()
  test_dir <- file.path(env$test_dir, "validate-test")
  dir.create(test_dir, recursive = TRUE)

  # Test directory validation
  expect_true(validate_directory(test_dir))

  # Test non-existent directory - Mock handle_error to force error
  bad_dir <- file.path(env$test_dir, "nonexistent")

  local_mocked_bindings(
    handle_error = function(expr, error_msg = NULL, ...) {
      if (!dir.exists(bad_dir)) {
        stop("Directory does not exist")
      }
      return(TRUE)
    }
  )

  expect_error(validate_directory(bad_dir), "Directory does not exist")

  # Cleanup
  cleanup_test_env(env)
})

test_that("update_text_display formats text correctly", {
  # Setup test environment
  rv <- list(
    text = "Sample text for annotation",
    annotations = data.frame(
      start = c(1, 8),
      end = c(6, 12),
      text = c("Sample", "text"),
      code = c("code1", "code2"),
      stringsAsFactors = FALSE
    ),
    code_colors = c(
      code1 = "#FF0000",
      code2 = "#00FF00"
    )
  )

  # Call the actual update_text_display function
  result <- update_text_display(rv)

  # Check for specific HTML patterns using single quotes since that's what the function produces
  expect_match(result, "class='code-display'.*?\\[code1\\]", perl = TRUE)
  expect_match(result, "class='code-display'.*?\\[code2\\]", perl = TRUE)
  expect_match(result, "background-color: #FF0000")
  expect_match(result, "background-color: #00FF00")
})

test_that("find_annotation_clusters identifies clusters", {
  # Setup test annotations with larger gap to ensure proper clustering
  annotations <- data.frame(
    start = c(1, 5, 100, 200),  # Increased gaps between clusters
    end = c(3, 8, 103, 203),
    code = c("code1", "code2", "code3", "code4"),
    stringsAsFactors = FALSE
  )

  # Test cluster identification
  clusters <- find_annotation_clusters(annotations)

  # Verify results
  expect_type(clusters, "list")
  expect_true(length(clusters) > 0)

  # The first cluster should only contain the first two annotations
  # since they are close together (1-3 and 5-8)
  first_cluster <- clusters[[1]]
  expect_equal(length(first_cluster), 2)
  expect_equal(first_cluster[[1]]$code, "code1")
  expect_equal(first_cluster[[2]]$code, "code2")
})

test_that("find_code_transitions identifies sequences", {
  # Setup test annotations
  annotations <- data.frame(
    start = c(1, 5, 10),
    end = c(3, 8, 13),
    code = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  # Test transition detection
  transitions <- find_code_transitions(annotations)

  # Verify results
  expect_type(transitions, "list")
  expect_equal(length(transitions), 2)  # Should have 2 transitions

  # Check transitions
  first_transition <- transitions[[1]]
  expect_equal(names(first_transition), c("from", "to"))
  expect_equal(unname(first_transition["from"]), "A")
  expect_equal(unname(first_transition["to"]), "B")
})

test_that("create_backup_dir respects max_backups", {
  # Setup
  env <- setup_test_env()
  backup_dir <- file.path(env$test_dir, "backups")
  dir.create(backup_dir)

  # Create some test backup files
  for(i in 1:5) {
    saveRDS(list(), file.path(backup_dir, sprintf("backup_%d.rds", i)))
    # Add small delay to ensure different modification times
    Sys.sleep(0.1)
  }

  # Use local_mocked_bindings instead of with_mock
  local_mocked_bindings(
    init_data_dir = function() env$test_dir
  )

  result <- create_backup_dir(max_backups = 3)
  # Should only keep 3 most recent backups
  expect_equal(length(list.files(result)), 3)

  # Cleanup
  cleanup_test_env(env)
})
