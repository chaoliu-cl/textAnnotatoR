#' Initialize data directory
#'
#' @description
#' Creates and initializes the package data directory in the user's home directory.
#' Uses R_user_dir to ensure compliance with CRAN policies regarding file system
#' access. All operations are performed only when explicitly requested by the user.
#'
#' @return Character string containing the path to the data directory
#' @importFrom tools R_user_dir
#' @importFrom shiny showNotification
#' @keywords internal
#' @examples
#' \dontrun{
#' # Initialize the data directory
#' data_dir <- init_data_dir()
#'
#' # Check if directory exists
#' dir.exists(data_dir)
#'
#' # Check directory contents
#' list.files(data_dir)
#' }
init_data_dir <- function() {
  handle_error(
    expr = {
      data_dir <- tools::R_user_dir("textAnnotatoR", "data")
      if (!dir.exists(data_dir)) {
        dir.create(data_dir, recursive = TRUE)
      }
      return(data_dir)
    },
    error_msg = "Failed to initialize data directory",
    success_msg = NULL  # Silent on success as this is an internal operation
  )
}

#' Get project directory
#'
#' @description
#' Returns the path to the projects directory, creating it if necessary.
#' This directory is used for storing saved annotation projects. The function
#' ensures the directory exists and is accessible before returning its path.
#'
#' @return Character string containing the path to the projects directory
#' @importFrom shiny showNotification
#' @keywords internal
#' @examples
#' \dontrun{
#' # Get the projects directory path
#' project_dir <- get_project_dir()
#'
#' # Save a project file
#' saveRDS(list(data = "example"),
#'         file = file.path(project_dir, "example_project.rds"))
#'
#' # List project files
#' list.files(project_dir, pattern = "\\.rds$")
#' }
get_project_dir <- function() {
  handle_error(
    expr = {
      data_dir <- init_data_dir()
      project_dir <- file.path(data_dir, "projects")
      if (!dir.exists(project_dir)) {
        dir.create(project_dir, recursive = TRUE)
      }
      return(project_dir)
    },
    error_msg = "Failed to create or access project directory",
    success_msg = NULL  # Silent on success
  )
}

#' Get export directory
#'
#' @description
#' Returns the path to the exports directory, creating it if necessary.
#' This directory is used for storing exported annotation data, visualizations,
#' and analysis results. The function ensures the directory exists and is
#' accessible before returning its path.
#'
#' @return Character string containing the path to the exports directory
#' @importFrom shiny showNotification
#' @keywords internal
#' @examples
#' \dontrun{
#' # Get the exports directory path
#' export_dir <- get_export_dir()
#'
#' # Export some data
#' write.csv(data.frame(x = 1:3, y = letters[1:3]),
#'           file = file.path(export_dir, "example_export.csv"))
#'
#' # Check exported files
#' list.files(export_dir, pattern = "\\.csv$")
#' }
get_export_dir <- function() {
  handle_error(
    expr = {
      data_dir <- init_data_dir()
      export_dir <- file.path(data_dir, "exports")
      if (!dir.exists(export_dir)) {
        dir.create(export_dir, recursive = TRUE)
      }
      return(export_dir)
    },
    error_msg = "Failed to create or access export directory",
    success_msg = NULL  # Silent on success
  )
}

#' Clean project path
#'
#' @description
#' Ensures project paths are valid and secure by removing potentially harmful
#' characters and preventing directory traversal attempts. This function
#' sanitizes file paths to prevent security issues and ensure consistent
#' file naming across different operating systems.
#'
#' @param path Character string containing the project path to be cleaned
#' @return Sanitized path string that is safe to use for file operations
#' @importFrom shiny showNotification
#' @keywords internal
#' @examples
#' \dontrun{
#' # Clean a simple filename
#' clean_project_path("my_project.rds")
#'
#' # Clean a path with special characters
#' clean_project_path("my@project#2023.rds")
#'
#' # Clean a path with directory traversal attempt
#' clean_project_path("../dangerous/path/project.rds")
#'
#' # Clean a path with spaces and special characters
#' clean_project_path("My Project 2023!.rds")
#' }
clean_project_path <- function(path) {
  handle_error(
    expr = {
      # Remove any potentially harmful characters and path traversal attempts
      path <- basename(path)  # Only take the filename part
      path <- gsub("[^[:alnum:]._-]", "", path)  # Remove special characters

      # Ensure the path is within the project directory
      cleaned_path <- file.path(get_project_dir(), path)

      if (nchar(cleaned_path) == 0) {
        stop("Invalid path: results in empty string after cleaning")
      }

      return(cleaned_path)
    },
    error_msg = "Failed to clean project path",
    success_msg = NULL  # Silent on success
  )
}

#' Validate directory access
#'
#' @description
#' Checks if a directory exists and is accessible for reading and writing.
#' This function performs comprehensive checks to ensure the directory
#' can be used for file operations.
#'
#' @param dir_path Character string containing the directory path to validate
#' @return Logical indicating whether the directory is valid and accessible
#' @importFrom shiny showNotification
#' @keywords internal
#' @examples
#' \dontrun{
#' # Validate project directory
#' project_dir <- get_project_dir()
#' is_valid <- validate_directory(project_dir)
#'
#' # Validate export directory
#' export_dir <- get_export_dir()
#' is_valid <- validate_directory(export_dir)
#' }
validate_directory <- function(dir_path) {
  handle_error(
    expr = {
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
    },
    error_msg = paste("Directory validation failed for:", dir_path),
    success_msg = NULL  # Silent on success
  )
}

#' Create backup directory
#'
#' @description
#' Creates and manages a backup directory for project files. This function
#' ensures that automatic backups have a designated storage location that
#' is properly managed and cleaned up when needed.
#'
#' @param max_backups Integer specifying maximum number of backups to keep
#' @return Character string containing the path to the backup directory
#' @importFrom shiny showNotification
#' @keywords internal
#' @examples
#' \dontrun{
#' # Create backup directory with default settings
#' backup_dir <- create_backup_dir()
#'
#' # Create backup directory with custom maximum
#' backup_dir <- create_backup_dir(max_backups = 5)
#'
#' # Check backup directory contents
#' list.files(backup_dir)
#' }
create_backup_dir <- function(max_backups = 3) {
  handle_error(
    expr = {
      data_dir <- init_data_dir()
      backup_dir <- file.path(data_dir, "backups")

      if (!dir.exists(backup_dir)) {
        dir.create(backup_dir, recursive = TRUE)
      }

      # Clean up old backups if necessary
      backup_files <- list.files(backup_dir, full.names = TRUE)
      if (length(backup_files) > max_backups) {
        file_info <- file.info(backup_files)
        file_info <- file_info[order(file_info$mtime), ]
        files_to_remove <- row.names(file_info)[1:(length(backup_files) - max_backups)]
        unlink(files_to_remove)
      }

      return(backup_dir)
    },
    error_msg = "Failed to create or manage backup directory",
    success_msg = NULL  # Silent on success
  )
}

#' Clean directory paths for export
#'
#' @description
#' Sanitizes directory paths for file export operations. This function ensures
#' that export paths are valid, accessible, and secure before files are written.
#' It handles path normalization and security checks for export operations.
#'
#' @param dir_path Character string containing the directory path to clean
#' @param create Logical indicating whether to create the directory if it doesn't exist
#' @return Cleaned and validated directory path
#' @importFrom shiny showNotification
#' @keywords internal
#' @examples
#' \dontrun{
#' # Clean export path
#' export_path <- clean_export_path("my/export/path")
#'
#' # Clean and create directory if needed
#' export_path <- clean_export_path("my/export/path", create = TRUE)
#' }
clean_export_path <- function(dir_path, create = FALSE) {
  handle_error(
    expr = {
      # Normalize path
      dir_path <- normalizePath(dir_path, mustWork = FALSE)

      # Ensure path is within allowed directories
      export_dir <- get_export_dir()
      if (!grepl(sprintf("^%s", export_dir), dir_path)) {
        dir_path <- file.path(export_dir, basename(dir_path))
      }

      # Create directory if requested and it doesn't exist
      if (create && !dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }

      # Validate directory
      if (!validate_directory(dir_path)) {
        stop("Directory validation failed")
      }

      return(dir_path)
    },
    error_msg = "Failed to clean export path",
    success_msg = NULL  # Silent on success
  )
}
