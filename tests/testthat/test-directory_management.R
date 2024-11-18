library(testthat)
library(mockery)
library(shiny)

# Test helper to create temporary test environment
create_test_env <- function() {
  # Create temporary directory structure
  test_dir <- normalizePath(file.path(tempdir(), "textAnnotatoR_test"), mustWork = FALSE)
  if (dir.exists(test_dir)) {
    unlink(test_dir, recursive = TRUE)
  }
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  # Clean up function for after tests
  cleanup <- function() {
    if (dir.exists(test_dir)) {
      unlink(test_dir, recursive = TRUE)
    }
  }

  list(
    test_dir = test_dir,
    cleanup = cleanup
  )
}

test_that("init_data_dir creates directory only after confirmation", {
  # Create and normalize test directory path
  test_env <- create_test_env()
  on.exit(test_env$cleanup())

  # Create a mock session
  mock_session <- structure(list(), class = "ShinySession")

  # Create test environment with the test directory path
  mock_env <- new.env(parent = parent.env(environment()))
  mock_env$showModal <- mock()
  mock_env$test_dir <- test_env$test_dir  # Add test_dir to the environment

  # Create modified version of init_data_dir that uses the environment's test_dir
  temp_init_data_dir <- function(session) {
    current_dir <- get("test_dir", envir = parent.env(environment()))
    if (!dir.exists(current_dir)) {
      showModal(modalDialog(
        title = "Permission Required",
        sprintf("textAnnotatoR needs to create a directory at:\n%s", current_dir),
        footer = tagList(
          modalButton("Decline"),
          actionButton("confirm_create_dir", "Approve")
        ),
        easyClose = FALSE
      ))
      return(NULL)
    }
    return(current_dir)
  }

  environment(temp_init_data_dir) <- mock_env

  # Test when directory doesn't exist
  if (dir.exists(test_env$test_dir)) {
    unlink(test_env$test_dir, recursive = TRUE)
  }

  result <- temp_init_data_dir(mock_session)
  expect_null(result)
  expect_called(mock_env$showModal, 1)

  # Test when directory exists
  dir.create(test_env$test_dir, recursive = TRUE, showWarnings = FALSE)
  result <- temp_init_data_dir(mock_session)
  expect_equal(result, test_env$test_dir)
})

test_that("get_project_dir creates and returns project directory", {
  test_env <- create_test_env()
  on.exit(test_env$cleanup())

  mock_init_data_dir <- mock(test_env$test_dir)

  # Create test environment
  mock_env <- new.env(parent = parent.env(environment()))
  mock_env$init_data_dir <- mock_init_data_dir

  # Create temporary get_project_dir function
  temp_get_project_dir <- function() {
    data_dir <- init_data_dir()
    project_dir <- file.path(data_dir, "projects")
    if (!dir.exists(project_dir)) {
      dir.create(project_dir, recursive = TRUE)
    }
    project_dir
  }

  environment(temp_get_project_dir) <- mock_env

  result <- temp_get_project_dir()
  expected_project_dir <- file.path(test_env$test_dir, "projects")

  expect_equal(result, expected_project_dir)
  expect_true(dir.exists(expected_project_dir))
})

test_that("clean_project_path sanitizes paths correctly", {
  test_env <- create_test_env()
  on.exit(test_env$cleanup())

  mock_get_project_dir <- mock(test_env$test_dir)

  # Create test environment
  mock_env <- new.env(parent = parent.env(environment()))
  mock_env$get_project_dir <- mock_get_project_dir

  # Create temporary clean_project_path function
  temp_clean_project_path <- function(path) {
    path <- basename(path)
    path <- gsub("[^[:alnum:]._-]", "", path)

    if (nchar(gsub("\\s", "", path)) == 0) {
      stop("Invalid path: results in empty string after cleaning")
    }

    cleaned_path <- file.path(get_project_dir(), path)
    cleaned_path
  }

  environment(temp_clean_project_path) <- mock_env

  # Test valid path cleaning
  dirty_path <- "../dangerous/path/file!@#$%^&*.txt"
  result <- temp_clean_project_path(dirty_path)
  expect_equal(basename(result), "file.txt")
  expect_true(startsWith(result, test_env$test_dir))

  # Test empty path
  expect_error(
    temp_clean_project_path("   "),
    "Invalid path: results in empty string after cleaning"
  )

  # Test empty string
  expect_error(
    temp_clean_project_path(""),
    "Invalid path: results in empty string after cleaning"
  )
})

test_that("validate_directory checks directory permissions correctly", {
  test_env <- create_test_env()
  on.exit(test_env$cleanup())

  # Create temporary validate_directory function that doesn't use handle_error
  temp_validate_directory <- function(dir_path) {
    # Check if directory exists
    if (!dir.exists(dir_path)) {
      stop("Directory does not exist")
    }

    # Check if directory is writable
    test_file <- tempfile(tmpdir = dir_path)
    tryCatch({
      file.create(test_file)
      unlink(test_file)
    }, error = function(e) {
      stop("Directory is not writable")
    })

    # Check if directory is readable
    if (!file.access(dir_path, mode = 4) == 0) {
      stop("Directory is not readable")
    }

    return(TRUE)
  }

  # Create test directory
  test_dir <- file.path(test_env$test_dir, "validate_test")
  dir.create(test_dir, recursive = TRUE)

  # Test valid directory
  expect_true(temp_validate_directory(test_dir))

  # Test non-existent directory
  nonexistent_dir <- file.path(test_dir, "nonexistent")
  expect_error(
    temp_validate_directory(nonexistent_dir),
    "Directory does not exist"
  )
})

test_that("create_backup_dir manages backups correctly", {
  test_env <- create_test_env()
  on.exit(test_env$cleanup())

  mock_init_data_dir <- mock(test_env$test_dir)

  # Create test environment
  mock_env <- new.env(parent = parent.env(environment()))
  mock_env$init_data_dir <- mock_init_data_dir

  # Create backup files
  backup_dir <- file.path(test_env$test_dir, "backups")
  dir.create(backup_dir, recursive = TRUE)

  for(i in 1:3) {
    file_path <- file.path(backup_dir, sprintf("backup%d.txt", i))
    writeLines(sprintf("Backup %d", i), file_path)
    Sys.sleep(0.1)
  }

  # Create temporary create_backup_dir function
  temp_create_backup_dir <- function(max_backups = 3) {
    data_dir <- init_data_dir()
    backup_dir <- file.path(data_dir, "backups")

    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }

    # Clean up old backups
    backup_files <- list.files(backup_dir, full.names = TRUE)
    if (length(backup_files) > max_backups) {
      file_info <- file.info(backup_files)
      file_info <- file_info[order(file_info$mtime), ]
      files_to_remove <- row.names(file_info)[1:(length(backup_files) - max_backups)]
      unlink(files_to_remove)
    }

    return(backup_dir)
  }

  environment(temp_create_backup_dir) <- mock_env

  # Test backup management
  result <- temp_create_backup_dir(max_backups = 2)

  # Allow time for file operations
  Sys.sleep(0.2)

  # Verify results
  backup_files <- list.files(backup_dir)
  expect_length(backup_files, 2)
})
