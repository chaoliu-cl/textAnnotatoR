#' Save Project State to File
#'
#' @param state List containing project state with elements:
#'   \itemize{
#'     \item text: Character string of document text
#'     \item annotations: Data frame of annotations
#'     \item codes: Character vector of code names
#'     \item code_tree: data.tree object of code hierarchy
#'   }
#' @param filename Character string for output filename
#' @return Invisible TRUE if successful, stops with error if failed
#' @examples
#' \dontrun{
#' state <- list(
#'   text = "Sample text",
#'   annotations = data.frame(
#'     start = c(1, 10),
#'     end = c(5, 15),
#'     code = c("Code1", "Code2"),
#'     stringsAsFactors = FALSE
#'   ),
#'   codes = c("Code1", "Code2"),
#'   code_tree = Node$new("Root")
#' )
#' save_project_state(state, "project1")
#' }
#' @importFrom jsonlite toJSON
#' @keywords internal
save_project_state <- function(state, filename) {
  # Validate inputs
  if (!is.list(state)) {
    stop("state must be a list")
  }
  required_elements <- c("text", "annotations", "codes", "code_tree")
  missing_elements <- setdiff(required_elements, names(state))
  if (length(missing_elements) > 0) {
    stop("state is missing required elements: ",
         paste(missing_elements, collapse = ", "))
  }
  if (!is.character(filename) || length(filename) != 1) {
    stop("filename must be a single character string")
  }

  # Ensure projects directory exists
  if (!dir.exists("projects")) {
    dir.create("projects")
  }

  # Convert data.tree object to list for serialization
  state$code_tree <- as.list(state$code_tree)

  # Save state
  tryCatch({
    saveRDS(state, file = file.path("projects", paste0(filename, ".rds")))
    invisible(TRUE)
  }, error = function(e) {
    stop("Failed to save project: ", e$message)
  })
}


#' Load Project State from File
#'
#' Loads a previously saved text annotation project state from a file. Handles
#' conversion of serialized data structures back to their original format,
#' including reconstruction of the code hierarchy tree.
#'
#' @param filename Character string specifying the name of the file to load (with or without .rds extension).
#'   The file should be located in the 'projects' subdirectory.
#' @return List containing the loaded project state with elements:
#'   \itemize{
#'     \item text: Character string of document text
#'     \item annotations: Data frame of annotations
#'     \item codes: Character vector of code names
#'     \item code_tree: data.tree object of code hierarchy
#'   }
#'   Returns NULL if the file cannot be loaded.
#' @examples
#' \dontrun{
#' # Load a project state
#' project <- load_project_state("project1.rds")
#'
#' # Check if loading was successful
#' if (!is.null(project)) {
#'   # Access project components
#'   print(nchar(project$text))  # Length of document
#'   print(nrow(project$annotations))  # Number of annotations
#'   print(length(project$codes))  # Number of codes
#'
#'   # Print code hierarchy
#'   print(project$code_tree, "name")
#' }
#'
#' # Example with error handling
#' tryCatch({
#'   project <- load_project_state("nonexistent.rds")
#'   if (is.null(project)) {
#'     stop("Project could not be loaded")
#'   }
#' }, error = function(e) {
#'   message("Error loading project: ", e$message)
#' })
#' }
#' @importFrom jsonlite fromJSON
#' @importFrom data.tree as.Node
#' @seealso \code{\link{save_project_state}} for saving projects
#' @keywords internal
load_project_state <- function(filename) {
  handle_error(
    expr = {
      state <- readRDS(file.path("projects", filename))
      # Convert list back to data.tree object
      state$code_tree <- as.Node(state$code_tree)
      return(state)
    },
    error_msg = "Failed to load project"
  )
}
