#' Initialize data directory with user confirmation and location choice
#'
#' @description
#' Creates and initializes the package data directory after obtaining explicit
#' user confirmation. Allows users to choose between the default location or
#' a custom directory of their choice.
#'
#' @param session Shiny session object for displaying confirmation dialog
#' @return Character string containing the path to the data directory, or NULL if declined
#' @importFrom tools R_user_dir
#' @importFrom shiny showModal modalDialog selectInput
#' @keywords internal
init_data_dir <- function(session = NULL) {
  # If session is NULL, use a default directory path
  if (is.null(session)) {
    default_dir <- tools::R_user_dir("textAnnotatoR", "data")
    if (!dir.exists(default_dir)) {
      dir.create(default_dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(default_dir)
  }

  default_dir <- tools::R_user_dir("textAnnotatoR", "data")
  session$userData$custom_dir <- NULL

  if (!dir.exists(default_dir)) {
    # Set up roots for shinyFiles
    roots <- c(Home = path.expand("~"))
    if (.Platform$OS.type == "windows") {
      roots <- c(roots, getVolumes()())
    }

    # Initialize directory chooser before showing the modal
    shinyFiles::shinyDirChoose(input = session$input,
                               id = "custom_dir_select",
                               roots = roots,
                               session = session,
                               restrictions = system.file(package = "base"))

    # Show confirmation dialog with location options
    showModal(modalDialog(
      title = "Choose Storage Location",
      tags$p("textAnnotatoR needs to create a directory to store your annotation projects."),
      selectInput("storage_location", "Select storage location:",
                  choices = c("Default Location" = "default",
                              "Custom Location" = "custom")),
      conditionalPanel(
        condition = "input.storage_location == 'custom'",
        tags$div(style = "margin-bottom: 15px;",
                 # KEY CHANGE: Use shinyDirButton instead of actionButton
                 shinyFiles::shinyDirButton("custom_dir_select",
                                            "Choose Directory",
                                            "Select Directory for Data Storage",
                                            icon = icon("folder-open")),
                 tags$div(id = "selected_dir_container", style = "margin-top: 10px;",
                          tags$p("Currently selected: ",
                                 tags$span(id = "custom_dir_path",
                                           style = "font-weight: bold;",
                                           "No directory selected"))
                 )
        )
      ),
      tags$div(id = "default_dir_info",
               conditionalPanel(
                 condition = "input.storage_location == 'default'",
                 tags$p("Default location: ",
                        tags$span(style = "font-weight: bold;", default_dir))
               )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_data_dir", "Confirm & Create Directory")
      ),
      easyClose = FALSE
    ))

    # Add JavaScript to handle interactivity and UI updates
    shinyjs::runjs("
      $(document).ready(function() {
        $('#storage_location').on('change', function() {
          if ($(this).val() === 'custom') {
            $('#selected_dir_container').show();
            $('#default_dir_info').hide();
          } else {
            $('#selected_dir_container').hide();
            $('#default_dir_info').show();
          }
        });

        // Initialize the UI based on current selection
        if ($('#storage_location').val() === 'custom') {
          $('#selected_dir_container').show();
          $('#default_dir_info').hide();
        } else {
          $('#selected_dir_container').hide();
          $('#default_dir_info').show();
        }
      });
    ")

    return(NULL) # Return NULL initially, actual creation happens after confirmation
  }

  return(default_dir)
}

#' Get project directory path
#'
#' @description
#' Retrieves or creates the project directory path where all project files will be stored.
#' Creates the directory if it doesn't exist.
#'
#' @param rv ReactiveValues object containing application state (optional)
#' @return Character string containing the project directory path, or NULL if creation fails
#' @importFrom shiny showNotification
#' @keywords internal
get_project_dir <- function(rv = NULL) {
  project_dir <- handle_error(
    expr = {
      # If rv is provided and has a data_dir, use that
      data_dir <- if (!is.null(rv) && !is.null(rv$data_dir)) {
        rv$data_dir
      } else {
        tools::R_user_dir("textAnnotatoR", "data")
      }

      project_dir <- file.path(data_dir, "projects")
      if (!dir.exists(project_dir)) {
        dir.create(project_dir, recursive = TRUE)
      }
      project_dir
    },
    error_msg = "Failed to create or access project directory"
  )
  return(project_dir)
}

#' Handle custom directory selection
#'
#' @description
#' Opens a directory selection dialog and updates the UI with the selected path.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param roots List of root directories for selection
#' @return Character string containing the selected directory path
#' @importFrom shiny observeEvent
#' @importFrom shinyFiles shinyDirChoose parseDirPath
#' @keywords internal
handle_custom_dir_selection <- function(input, output, session, roots) {
  # Initialize directory selection
  shinyDirChoose(input, "custom_dir_select", roots = roots, session = session)

  # Handle directory selection
  observeEvent(input$custom_dir_select, {
    selected_dir <- parseDirPath(roots, input$custom_dir_select)
    if (length(selected_dir) > 0) {
      session$userData$custom_dir <- selected_dir
      # Update UI to show selected path using JavaScript
      shinyjs::runjs(sprintf('document.getElementById("custom_dir_path").innerText = "%s";',
                             gsub("\\\\", "\\\\\\\\", selected_dir)))
    }
  })
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
get_export_dir <- function() {
  handle_error(
    expr = {
      data_dir <- tools::R_user_dir("textAnnotatoR", "data")
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
create_backup_dir <- function(max_backups = 3) {
  handle_error(
    expr = {
      data_dir <- tools::R_user_dir("textAnnotatoR", "data")
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
