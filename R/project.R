#' Handle errors with custom messages
#'
#' @description
#' Provides error handling with customizable success, error, and completion messages.
#' Wraps expressions in a tryCatch block and displays appropriate notifications.
#'
#' @param expr Expression to evaluate
#' @param success_msg Optional character string for success notification
#' @param error_msg Optional character string for error notification
#' @param finally_msg Optional character string for completion notification
#'
#' @return Result of the expression or NULL if error occurs
#'
#' @importFrom shiny showNotification
#' @keywords internal
handle_error <- function(expr, success_msg = NULL, error_msg = NULL, finally_msg = NULL) {
  is_shiny <- requireNamespace("shiny", quietly = TRUE) &&
    exists("session") &&
    !is.null(get0("session"))

  notify <- function(msg, type = "message") {
    if (is_shiny) {
      shiny::showNotification(msg, type = type)
    } else {
      message(msg)
    }
  }

  tryCatch({
    result <- expr
    if (!is.null(success_msg)) {
      notify(success_msg, "message")
    }
    return(result)
  }, error = function(e) {
    msg <- if (is.null(error_msg)) paste("Error:", e$message) else error_msg
    notify(msg, "error")
    return(NULL)
  }, finally = {
    if (!is.null(finally_msg)) {
      notify(finally_msg, "message")
    }
  })
}

#' Save project state to file
#'
#' @description
#' Saves the current project state including text, annotations, codes, code tree,
#' and other metadata to an RDS file. Creates the projects directory if it doesn't exist.
#'
#' @param state List containing project state elements:
#'   \itemize{
#'     \item text: Character string of the current text
#'     \item annotations: Data frame of annotations
#'     \item codes: Character vector of codes
#'     \item code_tree: Node object representing code hierarchy
#'     \item code_colors: Named vector of code colors
#'     \item memos: List of memos
#'     \item code_descriptions: List of code descriptions
#'   }
#' @param filename Character string specifying the filename for saving
#'
#' @return Invisible NULL. Called for side effect of saving project state.
#' @importFrom utils packageVersion
#' @keywords internal
save_project_state <- function(state, filename) {
  # Create the projects directory if it doesn't exist
  project_dir <- get_project_dir()
  if (is.null(project_dir)) return(invisible(NULL))

  # Add .rds extension if not present
  if (!grepl("\\.rds$", filename)) {
    filename <- paste0(filename, ".rds")
  }

  # Clean the path and get full filepath
  filepath <- file.path(project_dir, basename(filename))

  # Add version information
  state$version <- utils::packageVersion("textAnnotatoR")

  # Save state to RDS file
  handle_error(
    expr = saveRDS(state, file = filepath),
    success_msg = paste("Project saved successfully to", filepath),
    error_msg = "Failed to save project"
  )

  invisible(NULL)
}

#' Load project state from file
#'
#' @description
#' Loads a previously saved project state from an RDS file. Performs version checking
#' and data structure validation during the loading process.
#'
#' @param filename Character string specifying the filename to load
#'
#' @return List containing the loaded project state, or NULL if loading fails
#'
#' @importFrom data.tree as.Node
#' @importFrom utils packageVersion
#' @keywords internal
load_project_state <- function(filename) {
  # Add .rds extension if not present
  if (!grepl("\\.rds$", filename)) {
    filename <- paste0(filename, ".rds")
  }

  # Get the projects directory and create full filepath
  project_dir <- get_project_dir()
  if (is.null(project_dir)) return(NULL)

  filepath <- file.path(project_dir, basename(filename))

  if (!file.exists(filepath)) {
    showNotification(paste("Project file not found:", filepath), type = "error")
    return(NULL)
  }

  state <- handle_error(
    expr = {
      state <- readRDS(filepath)

      # Version check
      current_version <- utils::packageVersion("textAnnotatoR")
      if (!is.null(state$version) && state$version > current_version) {
        warning("Project was created with a newer version of textAnnotatoR")
      }

      # Convert list back to data.tree object if necessary
      if (!is.null(state$code_tree) && !inherits(state$code_tree, "Node")) {
        state$code_tree <- as.Node(state$code_tree)
      }

      state
    },
    error_msg = paste("Failed to load project from", filepath)
  )

  return(state)
}

#' Save annotated text as HTML document
#'
#' @description
#' Creates an HTML document containing the annotated text with proper styling
#' for code highlights and formatting.
#'
#' @param filename Character string specifying output file path
#' @param rv ReactiveValues object containing:
#'   \itemize{
#'     \item text: Original text content
#'     \item annotations: Data frame of annotations
#'     \item code_colors: Named character vector of code colors
#'   }
#'
#' @return Invisible NULL, called for side effect
#' @keywords internal
save_as_html <- function(filename, rv) {
  # Get the current state of the text display
  html_content <- update_text_display(rv)

  # Create a complete HTML document
  full_html <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<style>\n",
    ".code-display { padding: 2px 5px; margin-right: 5px; border-radius: 3px; font-weight: bold; color: black; }\n",
    "</style>\n",
    "</head>\n<body>\n",
    "<h1>Annotated Text</h1>\n",
    "<div id='annotated_text'>\n",
    html_content,
    "\n</div>\n",
    "</body>\n</html>"
  )

  # Write the HTML content to a file
  writeLines(full_html, filename)
}

#' Save annotated text as plain text
#'
#' @description
#' Creates a plain text file containing the annotated text with code markers.
#'
#' @param filename Character string specifying output file path
#' @param rv ReactiveValues object containing:
#'   \itemize{
#'     \item text: Original text content
#'     \item annotations: Data frame of annotations
#'   }
#'
#' @return Invisible NULL, called for side effect
#' @keywords internal
save_as_text <- function(filename, rv) {
  # Get the annotated text
  annotated_text <- create_plain_text_annotations(rv$text, rv$annotations)

  # Write the content to a file
  writeLines(annotated_text, filename)
}

#' Enhanced save project interactive dialog
#'
#' @description
#' Shows modal dialog for saving project with options based on the selected storage mode.
#' For project-specific mode, always requires directory selection.
#'
#' @param rv ReactiveValues object containing project state
#' @param input Shiny input values
#' @param session Shiny session object
#' @param roots List of available storage volumes for directory selection
#' @return Invisible NULL, called for side effect
#' @keywords internal
save_project_interactive <- function(rv, input, session, roots) {
  # Determine if this is a "Save As" scenario (project already exists)
  is_save_as <- !is.null(rv$current_project) && !is.null(rv$current_project_path)
  
  # Determine if directory selection is needed
  needs_dir_selection <- rv$storage_mode == "project"
  
  # Get current project directory
  project_dir <- if (!is.null(rv$storage_mode) && rv$storage_mode != "project") {
    get_project_dir(rv)
  } else {
    "Project-specific storage: You'll need to select a location"
  }
  
  showModal(modalDialog(
    title = if (is_save_as) "Save Project As" else "Save Project",
    
    # Show current project info if this is a Save As
    if (is_save_as) {
      div(
        style = "margin-bottom: 15px; padding: 10px; background-color: #e8f4e8; border-radius: 4px;",
        tags$p(
          tags$strong("Current Project: "), rv$current_project,
          tags$br(),
          tags$small("Currently saved at: ", rv$current_project_path)
        )
      )
    },
    
    textInput("project_name", "Project Name:",
              value = rv$current_project %||% ""),
    
    # Show current storage mode info
    tags$div(
      style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
      tags$p(
        tags$strong("Storage Mode: "),
        if (!is.null(rv$storage_mode)) {
          switch(rv$storage_mode,
                 "default" = "Default location (R user directory)",
                 "custom" = "Custom directory",
                 "project" = "Project-specific storage")
        } else {
          "Default location"
        }
      ),
      tags$p(tags$small(project_dir))
    ),
    
    # Only show directory selection for project-specific mode or when requested
    if (needs_dir_selection) {
      div(style = "margin: 10px 0;",
          shinyDirButton("directory_select",
                         label = "Choose Directory",
                         title = "Select Directory to Save Project")
      )
    },
    
    if (needs_dir_selection) {
      verbatimTextOutput("selected_dir")
    },
    
    footer = tagList(
      modalButton("Cancel"),
      # Add Save As button if project already exists
      if (is_save_as && !needs_dir_selection) {
        actionButton("save_as_project", "Save As New Copy")
      },
      actionButton("confirm_save_project", if (is_save_as) "Save As" else "Save")
    )
  ))
}

#' Enhanced project loading functionality
#'
#' @description
#' Provides an improved project loading dialog with options to browse for project files
#' in any location, supporting the project-specific storage model.
#'
#' @param rv ReactiveValues object for project state
#' @param input Shiny input values
#' @param session Shiny session object
#' @param roots List of root directories for file selection
#'
#' @return Invisible NULL, called for side effect
#' @keywords internal
load_project_interactive <- function(rv, input, session, roots) {
  showModal(modalDialog(
    title = "Load Project",
    tags$p("Select a project file to load:"),
    
    # Storage mode dependent options
    if (!is.null(rv$storage_mode) && rv$storage_mode != "project") {
      # For default/custom modes, show recent projects first (if any)
      div(
        style = "margin-bottom: 20px;",
        tags$p(tags$strong("Recent Projects:")),
        uiOutput("recent_projects_list")
      )
    },
    
    # Always show file browser for maximum flexibility
    div(style = "margin: 15px 0;",
        shinyFilesButton("file_select",
                         label = "Browse for Project File",
                         title = "Select Project File",
                         multiple = FALSE)
    ),
    
    tags$p("Selected file:"),
    verbatimTextOutput("selected_file"),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_load_project", "Load")
    )
  ))
}

