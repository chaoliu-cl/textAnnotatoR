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
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- handle_error(
#'   expr = { 1 + 1 },
#'   success_msg = "Calculation completed"
#' )
#'
#' # With error handling
#' result <- handle_error(
#'   expr = { 1 / 0 },
#'   error_msg = "Division by zero detected",
#'   finally_msg = "Operation finished"
#' )
#' }
#'
#' @keywords internal
handle_error <- function(expr, success_msg = NULL, error_msg = NULL, finally_msg = NULL) {
  tryCatch({
    result <- expr
    if (!is.null(success_msg)) {
      showNotification(success_msg, type = "message")
    }
    return(result)
  }, error = function(e) {
    msg <- if (is.null(error_msg)) paste("Error:", e$message) else error_msg
    showNotification(msg, type = "error")
    return(NULL)
  }, finally = {
    if (!is.null(finally_msg)) {
      showNotification(finally_msg, type = "message")
    }
  })
}

#' Get project directory path
#'
#' @description
#' Retrieves or creates the project directory path where all project files will be stored.
#' Creates the directory if it doesn't exist.
#'
#' @return Character string containing the project directory path, or NULL if creation fails
#'
#' @examples
#' \dontrun{
#' # Get project directory
#' project_dir <- get_project_dir()
#'
#' # Check if directory exists
#' if (!is.null(project_dir)) {
#'   list.files(project_dir)
#' }
#' }
#'
#' @importFrom shiny showNotification
#' @keywords internal
get_project_dir <- function() {
  project_dir <- handle_error(
    expr = {
      data_dir <- init_data_dir()
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
#'
#' @examples
#' \dontrun{
#' state <- list(
#'   text = "Sample text",
#'   annotations = data.frame(
#'     start = 1,
#'     end = 5,
#'     code = "code1",
#'     stringsAsFactors = FALSE
#'   ),
#'   codes = "code1",
#'   code_tree = Node$new("Root"),
#'   code_colors = c(code1 = "#FF0000"),
#'   memos = list(),
#'   code_descriptions = list()
#' )
#' save_project_state(state, "my_project")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#' # Load a project
#' project_state <- load_project_state("project_name")
#'
#' # Check loaded components
#' str(project_state$text)
#' str(project_state$annotations)
#' }
#'
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
#'
#' @examples
#' \dontrun{
#' # Create HTML with annotations
#' rv <- list(
#'   text = "Sample text",
#'   annotations = data.frame(
#'     start = 1, end = 6,
#'     code = "code1"
#'   ),
#'   code_colors = c(code1 = "#FF0000")
#' )
#' save_as_html("output.html", rv)
#' }
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
#'
#' @examples
#' \dontrun{
#' rv <- list(
#'   text = "Sample text",
#'   annotations = data.frame(
#'     start = 1, end = 6,
#'     code = "code1"
#'   )
#' )
#' save_as_text("output.txt", rv)
#' }
#' @keywords internal
save_as_text <- function(filename, rv) {
  # Get the annotated text
  annotated_text <- create_plain_text_annotations(rv$text, rv$annotations)

  # Write the content to a file
  writeLines(annotated_text, filename)
}

#' Create plain text version of annotations
#'
#' @description
#' Converts annotated text to plain text format with code markers. Each annotation
#' is represented as a code identifier and annotated text wrapped in square brackets.
#' Multiple annotations are preserved and shown in order of appearance in the text.
#'
#' @param text Character string containing the original text
#' @param annotations Data frame of annotations with columns:
#'   \itemize{
#'     \item start: Numeric vector of starting positions
#'     \item end: Numeric vector of ending positions
#'     \item code: Character vector of code names
#'   }
#'
#' @return Character string containing formatted text with code markers
#'
#' @examples
#' \dontrun{
#' # Simple example with one annotation
#' text <- "This is a sample text"
#' annotations <- data.frame(
#'   start = 1,
#'   end = 4,
#'   code = "code1",
#'   stringsAsFactors = FALSE
#' )
#' result <- create_plain_text_annotations(text, annotations)
#' # Result will be: "[code1: This] is a sample text"
#'
#' # Example with multiple annotations
#' text <- "The quick brown fox"
#' annotations <- data.frame(
#'   start = c(1, 10),
#'   end = c(3, 15),
#'   code = c("article", "adjective"),
#'   stringsAsFactors = FALSE
#' )
#' result <- create_plain_text_annotations(text, annotations)
#' # Result will be: "[article: The] quick [adjective: brown fox]"
#' }
#'
#' @keywords internal
create_plain_text_annotations <- function(text, annotations) {
  if (nrow(annotations) == 0) {
    return(text)
  }

  sorted_annotations <- annotations[order(annotations$start), ]
  plain_text <- ""
  last_end <- 0

  for (i in 1:nrow(sorted_annotations)) {
    if (sorted_annotations$start[i] > last_end + 1) {
      plain_text <- paste0(plain_text, substr(text, last_end + 1, sorted_annotations$start[i] - 1))
    }
    plain_text <- paste0(plain_text,
                         "[", sorted_annotations$code[i], ": ",
                         substr(text, sorted_annotations$start[i], sorted_annotations$end[i]),
                         "]")
    last_end <- sorted_annotations$end[i]
  }

  if (last_end < nchar(text)) {
    plain_text <- paste0(plain_text, substr(text, last_end + 1, nchar(text)))
  }

  return(plain_text)
}

#' Display interactive project save dialog
#'
#' @description
#' Shows modal dialog for saving project with directory selection and
#' project name input.
#'
#' @param rv ReactiveValues object containing project state
#' @param input Shiny input values
#' @param session Shiny session object
#' @param volumes List of available storage volumes
#'
#' @return Invisible NULL, called for side effect
#'
#' @examples
#' \dontrun{
#' # In Shiny server function:
#' observeEvent(input$save_button, {
#'   save_project_interactive(rv, input, session, volumes)
#' })
#' }
#' @keywords internal
save_project_interactive <- function(rv, input, session, volumes) {
  showModal(modalDialog(
    title = "Save Project",
    textInput("project_name", "Project Name:",
              value = rv$current_project %||% ""),
    # Use regular file input instead of shinyFilesButton
    div(style = "margin: 10px 0;",
        shinyDirButton("directory_select",
                       label = "Choose Directory",
                       title = "Select Directory to Save Project")
    ),
    verbatimTextOutput("selected_dir"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_save_project", "Save")
    )
  ))
}

#' Display interactive project load dialog
#'
#' @description
#' Shows modal dialog for loading project with file selection functionality.
#'
#' @param rv ReactiveValues object for project state
#' @param input Shiny input values
#' @param session Shiny session object
#' @param roots List of root directories for file selection
#'
#' @return Invisible NULL, called for side effect
#'
#' @examples
#' \dontrun{
#' # In Shiny server function:
#' observeEvent(input$load_button, {
#'   load_project_interactive(rv, input, session, roots)
#' })
#' }
#' @keywords internal
load_project_interactive <- function(rv, input, session, roots) {
  showModal(modalDialog(
    title = "Load Project",
    div(style = "margin: 10px 0;",
        shinyFilesButton("file_select",
                         label = "Choose Project File",
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

#' Initialize new project
#'
#' @description
#' Creates new project by resetting all reactive values to defaults
#' and clearing UI elements.
#'
#' @param rv ReactiveValues object to reset containing:
#'   \itemize{
#'     \item text: Text content
#'     \item annotations: Annotation data frame
#'     \item codes: Vector of codes
#'     \item code_tree: Hierarchy Node object
#'     \item All other project state values
#'   }
#' @param session Shiny session object
#'
#' @return Invisible NULL, called for side effect
#'
#' @examples
#' \dontrun{
#' # In Shiny server:
#' observeEvent(input$new_project, {
#'   create_new_project(rv, session)
#' })
#' }
#' @keywords internal
create_new_project <- function(rv, session) {
  rv$text <- ""
  rv$annotations <- data.frame(
    start = integer(),
    end = integer(),
    text = character(),
    code = character(),
    memo = character(),
    stringsAsFactors = FALSE
  )
  rv$codes <- character()
  rv$code_tree <- Node$new("Root")
  rv$code_colors <- character()
  rv$memos <- list()
  rv$code_descriptions <- list()
  rv$history <- list(list(text = "", annotations = data.frame()))
  rv$history_index <- 1
  rv$current_project <- NULL
  rv$project_modified <- FALSE
  rv$action_history <- list()
  rv$action_index <- 0
  rv$merged_codes <- list()

  # Clear UI elements
  updateTextAreaInput(session, "text_input", value = "")
  session$sendCustomMessage("clearSelection", list())

  showNotification("New project created", type = "message")
}
