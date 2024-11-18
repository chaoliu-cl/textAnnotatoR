#' Null Coalescing Operator
#'
#' Implements a null coalescing operator similar to those found in other
#' programming languages. Returns the first non-NULL value between two options.
#'
#' @param a First value to check
#' @param b Default value to use if a is NULL
#' @return Returns a if not NULL, otherwise returns b
#' @examples
#' # Basic usage
#' x <- NULL
#' y <- 5
#' result1 <- x %||% y  # Returns 5
#' result2 <- y %||% x  # Returns 5
#'
#' # With more complex objects
#' list_a <- NULL
#' list_b <- list(value = 10)
#' result3 <- list_a %||% list_b  # Returns list(value = 10)
#'
#' # In a function
#' get_value <- function(input = NULL) {
#'   input %||% "default"
#' }
#' get_value()  # Returns "default"
#' get_value("custom")  # Returns "custom"
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Handle Error with Notifications
#'
#' Executes an expression with error handling and optional notifications for
#' success, error, and completion states. Designed for use in Shiny applications.
#'
#' @param expr Expression to evaluate
#' @param success_msg Optional success message (character string) to display on successful execution
#' @param error_msg Optional error message (character string) to display on error
#' @param finally_msg Optional message (character string) to display after execution, regardless of outcome
#' @return If successful, returns the result of expr. If failed, returns NULL.
#' @examples
#' \dontrun{
#' # Simple calculation with success message
#' result <- handle_error(
#'   expr = { 2 + 2 },
#'   success_msg = "Calculation completed",
#'   error_msg = "Calculation failed"
#' )
#'
#' # Error handling with custom messages
#' result <- handle_error(
#'   expr = {
#'     if (runif(1) > 0.5) stop("Random error")
#'     "Success!"
#'   },
#'   error_msg = "Operation failed - please try again",
#'   finally_msg = "Operation attempt completed"
#' )
#'
#' # File operation example
#' result <- handle_error(
#'   expr = {
#'     readLines("example.txt")
#'   },
#'   success_msg = "File read successfully",
#'   error_msg = "Could not read file",
#'   finally_msg = "File operation completed"
#' )
#' }
#' @importFrom shiny showNotification
#' @keywords internal
handle_error <- function(expr, success_msg = NULL, error_msg = NULL, finally_msg = NULL) {
  # Validate input parameters
  if (!is.null(success_msg) && !is.character(success_msg)) {
    stop("success_msg must be NULL or a character string")
  }
  if (!is.null(error_msg) && !is.character(error_msg)) {
    stop("error_msg must be NULL or a character string")
  }
  if (!is.null(finally_msg) && !is.character(finally_msg)) {
    stop("finally_msg must be NULL or a character string")
  }

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
