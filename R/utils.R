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

#' Create and manage undo/redo action
#'
#' @description
#' Creates an action object for the undo/redo system, containing information about
#' the type of action, the data involved, and how to reverse the action.
#'
#' @param type Character string specifying the type of action
#' @param data List containing the action data
#' @param reverse_data Optional list containing data for reversing the action
#'
#' @return List containing:
#'   \itemize{
#'     \item type: Action type identifier
#'     \item data: Action data
#'     \item reverse_data: Data for reversing the action
#'     \item timestamp: Time the action was created
#'   }
#' @keywords internal
create_action <- function(type, data, reverse_data = NULL) {
  list(
    type = type,
    data = data,
    reverse_data = reverse_data,
    timestamp = Sys.time()
  )
}

#' Concatenate memo texts
#'
#' @description
#' Combines existing and new memo texts with proper separators,
#' handling empty memos appropriately.
#'
#' @param existing_memo Character string containing current memo text
#' @param new_memo Character string containing memo text to append
#'
#' @return Character string of combined memo text
#' @keywords internal
concatenate_memos <- function(existing_memo, new_memo) {
  if (existing_memo == "") {
    return(new_memo)
  } else {
    return(paste(existing_memo, new_memo, sep = "; "))
  }
}

#' \%||\% operator
#'
#' @name grapes-or-or-grapes
#' @aliases %||%
#' @title Null coalescing operator
#' @description Provides null coalescing functionality, returning the first non-NULL argument
#' @param a First value to check
#' @param b Second value (default) to use if first is NULL
#' @return Returns \code{a} if not NULL, otherwise returns \code{b}
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Get HTML entity for folder icon
#'
#' @description
#' Returns the HTML entity code for an open folder icon
#'
#' @return Character string containing HTML entity
#' @keywords internal
get_folder_icon <- function() {
  "&#128194;" # Folder open icon HTML entity
}

#' Get HTML entity for file icon
#'
#' @description
#' Returns the HTML entity code for a file/document icon
#'
#' @return Character string containing HTML entity
#' @keywords internal
get_file_icon <- function() {
  "&#128196;" # File icon HTML entity
}

#' Get HTML entity for closed folder icon
#'
#' @description
#' Returns the HTML entity code for a closed folder icon
#'
#' @return Character string containing HTML entity
#' @keywords internal
get_closed_folder_icon <- function() {
  "&#128193;" # Closed folder icon HTML entity
}

#' Get HTML entity for calendar icon
#'
#' @description
#' Returns the HTML entity code for a calendar icon
#'
#' @return Character string containing HTML entity
#' @keywords internal
get_calendar_icon <- function() {
  "&#128197;" # Calendar icon HTML entity
}
