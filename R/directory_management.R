#' Initialize data directory with user options
#'
#' @description
#' Provides users with options for where to store annotation data:
#' 1. User's home directory (default R_user_dir location)
#' 2. Custom directory selected by the user
#' 3. Project-specific directory (for per-project storage)
#'
#' @param session Shiny session object for displaying dialog
#' @return Character string containing the path to the selected data directory, or NULL if declined
#' @importFrom tools R_user_dir
#' @importFrom shiny showModal modalDialog
#' @keywords internal
init_data_dir <- function(session) {
  # Default directory using R_user_dir
  default_dir <- tools::R_user_dir("textAnnotatoR", "data")
  
  # Show options dialog
  showModal(modalDialog(
    title = "Data Storage Location",
    tags$div(
      tags$p("Please select where you would like to store your annotation data:"),
      
      # Option 1: Default location
      radioButtons("storage_location", "Storage Location:",
                   choices = list(
                     "Default location (recommended)" = "default",
                     "Custom directory" = "custom",
                     "Project-specific (store with each project)" = "project"
                   ),
                   selected = "default"),
      
      # Show the default path
      conditionalPanel(
        condition = "input.storage_location == 'default'",
        tags$div(
          style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
          tags$p(
            tags$strong("Default Location: "),
            default_dir
          ),
          tags$p("This location will be used to store all your annotation projects.")
        )
      ),
      
      # Custom directory selection
      conditionalPanel(
        condition = "input.storage_location == 'custom'",
        tags$div(
          shinyDirButton("custom_dir_select", 
                         label = "Choose Custom Directory",
                         title = "Select Directory for Data Storage"),
          verbatimTextOutput("selected_custom_dir")
        )
      ),
      
      # Project-specific explanation
      conditionalPanel(
        condition = "input.storage_location == 'project'",
        tags$div(
          style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
          tags$p("When you save a project, you'll be asked where to store all related data."),
          tags$p("This option gives you maximum flexibility but requires you to manage project locations yourself.")
        )
      )
    ),
    footer = tagList(
      modalButton("Decline"),
      actionButton("confirm_storage_location", "Approve")
    ),
    easyClose = FALSE
  ))
  
  return(NULL) # Return NULL initially, actual directory creation happens after confirmation
}

#' Handle directory storage confirmation
#'
#' @description
#' Processes the user's storage location choice and creates directories as needed
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param rv ReactiveValues object containing application state
#' @param session Shiny session object
#' @param roots List of root directories for selection
#' @keywords internal
handle_storage_confirmation <- function(input, output, rv, session, roots) {
  observeEvent(input$confirm_storage_location, {
    # Process based on selected storage option
    if (input$storage_location == "default") {
      # Use default R_user_dir location
      data_dir <- tools::R_user_dir("textAnnotatoR", "data")
      
      tryCatch({
        dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
        rv$data_dir <- data_dir
        rv$storage_mode <- "default"
        removeModal()
        showNotification("Default directory created successfully", type = "message")
      }, error = function(e) {
        showNotification(
          sprintf("Failed to create directory: %s", e$message),
          type = "error"
        )
      })
      
    } else if (input$storage_location == "custom") {
      # Use user-selected custom directory
      if (!is.null(input$custom_dir_select)) {
        custom_dir <- parseDirPath(roots, input$custom_dir_select)
        
        if (dir.exists(custom_dir)) {
          # Create a subdirectory for textAnnotatoR
          data_dir <- file.path(custom_dir, "textAnnotatoR_data")
          
          tryCatch({
            dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
            rv$data_dir <- data_dir
            rv$storage_mode <- "custom"
            removeModal()
            showNotification(paste("Custom directory created at:", data_dir), type = "message")
          }, error = function(e) {
            showNotification(
              sprintf("Failed to create directory: %s", e$message),
              type = "error"
            )
          })
        } else {
          showNotification("Please select a valid directory", type = "warning")
        }
      } else {
        showNotification("Please select a custom directory", type = "warning")
      }
      
    } else if (input$storage_location == "project") {
      # Use project-specific storage (set in save_project_interactive)
      rv$storage_mode <- "project"
      removeModal()
      showNotification("Project-specific storage selected. You'll choose locations when saving projects.", 
                       type = "message")
    }
  })
  
  # Display selected custom directory
  output$selected_custom_dir <- renderText({
    if (!is.null(input$custom_dir_select)) {
      parseDirPath(roots, input$custom_dir_select)
    }
  })
}

#' Get project directory path based on storage mode
#'
#' @description
#' Retrieves or creates the project directory path where project files will be stored,
#' taking into account the user's selected storage mode.
#'
#' @param rv ReactiveValues object containing application state (optional)
#' @return Character string containing the project directory path, or NULL if not applicable
#' @importFrom shiny showNotification
#' @keywords internal
get_project_dir <- function(rv = NULL) {
  # If rv is provided and contains storage mode info
  if (!is.null(rv) && !is.null(rv$storage_mode)) {
    if (rv$storage_mode == "project") {
      # For project-specific mode, return NULL as the directory will be chosen at save time
      return(NULL)
    } else if (rv$storage_mode == "custom" && !is.null(rv$data_dir)) {
      # For custom mode, use the stored custom directory
      project_dir <- file.path(rv$data_dir, "projects")
      
      if (!dir.exists(project_dir)) {
        dir.create(project_dir, recursive = TRUE)
      }
      
      return(project_dir)
    }
  }
  
  # Default behavior (or fallback) - use R_user_dir
  handle_error(
    expr = {
      data_dir <- tools::R_user_dir("textAnnotatoR", "data")
      project_dir <- file.path(data_dir, "projects")
      
      if (!dir.exists(project_dir)) {
        dir.create(project_dir, recursive = TRUE)
      }
      
      return(project_dir)
    },
    error_msg = "Failed to create or access project directory"
  )
}

#' Enhanced save project confirmation handler
#'
#' @description
#' Processes the project save confirmation based on storage mode
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param rv ReactiveValues object containing application state
#' @param roots List of root directories for selection
#' @keywords internal
handle_save_project_confirmation <- function(input, output, session, rv, roots) {
  # Display selected save directory
  output$selected_dir <- renderText({
    if (!is.null(input$directory_select)) {
      parseDirPath(roots, input$directory_select)
    }
  })
  
  # Handle project save confirmation
  observeEvent(input$confirm_save_project, {
    req(input$project_name)
    
    # Handle based on storage mode
    if (!is.null(rv$storage_mode) && rv$storage_mode == "project") {
      # Project-specific mode - require directory selection
      req(input$directory_select)
      
      # Get selected directory path
      dir_path <- parseDirPath(roots, input$directory_select)
      
      # Construct full filepath
      filename <- if (!grepl("\\.rds$", input$project_name)) {
        paste0(input$project_name, ".rds")
      } else {
        input$project_name
      }
      filepath <- file.path(dir_path, filename)
      
      # Create project subdirectory
      project_dir <- file.path(dir_path, "textAnnotatoR_project_data")
      if (!dir.exists(project_dir)) {
        dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      # Create project state
      project_state <- list(
        text = rv$text,
        annotations = rv$annotations,
        codes = rv$codes,
        code_tree = rv$code_tree,
        code_colors = rv$code_colors,
        memos = rv$memos,
        code_descriptions = rv$code_descriptions,
        history = rv$history,
        history_index = rv$history_index,
        project_dir = project_dir  # Store project-specific directory
      )
      
      # Save project
      tryCatch({
        saveRDS(project_state, file = filepath)
        rv$current_project <- input$project_name
        rv$current_project_path <- filepath
        rv$project_specific_dir <- project_dir
        rv$project_modified <- FALSE
        showNotification(paste("Project saved successfully to", filepath), type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving project:", e$message), type = "error")
      })
      
    } else {
      # Default or custom mode - use predefined directory
      project_dir <- get_project_dir(rv)
      
      # Construct full filepath
      filename <- if (!grepl("\\.rds$", input$project_name)) {
        paste0(input$project_name, ".rds")
      } else {
        input$project_name
      }
      filepath <- file.path(project_dir, filename)
      
      # Create project state
      project_state <- list(
        text = rv$text,
        annotations = rv$annotations,
        codes = rv$codes,
        code_tree = rv$code_tree,
        code_colors = rv$code_colors,
        memos = rv$memos,
        code_descriptions = rv$code_descriptions,
        history = rv$history,
        history_index = rv$history_index
      )
      
      # Save project
      tryCatch({
        saveRDS(project_state, file = filepath)
        rv$current_project <- input$project_name
        rv$current_project_path <- filepath
        rv$project_modified <- FALSE
        showNotification(paste("Project saved successfully to", filepath), type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving project:", e$message), type = "error")
      })
    }
    
    removeModal()
  })
}

#' Enhanced project loading confirmation handler
#'
#' @description
#' Handles the loading of projects with support for different storage modes
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param rv ReactiveValues object containing application state
#' @param roots List of root directories for selection
#' @keywords internal
handle_load_project_confirmation <- function(input, output, session, rv, roots) {
  # Display selected file
  output$selected_file <- renderText({
    if (!is.null(input$file_select)) {
      selected <- parseFilePaths(roots, input$file_select)
      if (nrow(selected) > 0) {
        as.character(selected$datapath)
      }
    }
  })
  
  # Recent projects list
  output$recent_projects_list <- renderUI({
    project_dir <- get_project_dir(rv)
    
    if (is.null(project_dir) || !dir.exists(project_dir)) {
      return(tags$p("No recent projects found"))
    }
    
    # List RDS files in the project directory
    project_files <- list.files(project_dir, pattern = "\\.rds$", full.names = FALSE)
    
    if (length(project_files) == 0) {
      return(tags$p("No recent projects found"))
    }
    
    # Create action buttons for each project
    project_buttons <- lapply(project_files, function(file) {
      project_name <- tools::file_path_sans_ext(file)
      actionButton(
        inputId = paste0("load_project_", gsub("[^a-zA-Z0-9]", "_", project_name)),
        label = project_name,
        onclick = sprintf("Shiny.setInputValue('project_to_load', '%s');", project_name),
        class = "btn-sm btn-light",
        style = "margin: 2px; text-align: left; width: 100%;"
      )
    })
    
    # Return as a div
    div(
      style = "max-height: 200px; overflow-y: auto; border: 1px solid #eee; padding: 5px;",
      project_buttons
    )
  })
  
  # Handle loading from recent projects list
  observeEvent(input$project_to_load, {
    if (!is.null(input$project_to_load)) {
      if (rv$project_modified) {
        showModal(modalDialog(
          title = "Save Current Project?",
          "Would you like to save the current project before loading another?",
          footer = tagList(
            actionButton("save_before_load", "Save First"),
            actionButton("load_without_saving", "Don't Save"),
            modalButton("Cancel")
          )
        ))
      } else {
        # Call the function with the correct arguments
        load_selected_project(rv, input, session)
      }
    }
  })
  
  # Handle confirmation of loading from file browser
  observeEvent(input$confirm_load_project, {
    req(input$file_select)
    
    # Get selected file path
    selected <- parseFilePaths(roots, input$file_select)
    if (nrow(selected) == 0) return()
    filepath <- as.character(selected$datapath[1])
    
    if (rv$project_modified) {
      # Store the filepath and show save confirmation
      rv$pending_load_path <- filepath
      showModal(modalDialog(
        title = "Save Current Project?",
        "Would you like to save the current project before loading another?",
        footer = tagList(
          actionButton("save_before_load_file", "Save First"),
          actionButton("load_file_without_saving", "Don't Save"),
          modalButton("Cancel")
        )
      ))
    } else {
      # Load directly
      load_project_from_path(rv, filepath, session)
    }
  })
  
  # Handle loading without saving (for recent projects)
  observeEvent(input$load_without_saving, {
    removeModal()
    # Call the function with the correct arguments
    load_selected_project(rv, input, session)
  })
  
  # Handle loading without saving (for file browser)
  observeEvent(input$load_file_without_saving, {
    removeModal()
    load_project_from_path(rv, rv$pending_load_path, session)
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
