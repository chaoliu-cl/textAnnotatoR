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

#' Apply or reverse an action
#'
#' @description
#' Applies or reverses an action in the undo/redo system. Handles different types of
#' actions including adding/removing annotations and merging/unmerging codes.
#'
#' @param rv ReactiveValues object containing application state
#' @param action List containing action information
#' @param reverse Logical indicating whether to reverse the action
#'
#' @return Invisible rv (ReactiveValues object)
#' @keywords internal
apply_action <- function(rv, action, reverse = FALSE) {
  data <- if (reverse) action$reverse_data else action$data

  switch(action$type,
         "add_annotation" = {
           if (reverse) {
             # Remove annotation
             if(nrow(rv$annotations) > 0) {
               rv$annotations <- rv$annotations[-which(
                 rv$annotations$start == data$start &
                   rv$annotations$end == data$end &
                   rv$annotations$code == data$code
               ), ]
             }
           } else {
             # Add annotation
             if(is.null(rv$annotations)) {
               rv$annotations <- data.frame(
                 start = integer(),
                 end = integer(),
                 code = character(),
                 stringsAsFactors = FALSE
               )
             }
             rv$annotations <- rbind(rv$annotations, data)
           }
         },
         "merge_codes" = {
           if (reverse) {
             # Reverse the merge
             indices <- which(rv$annotations$code == data$new_code)
             if (length(indices) > 0) {
               # Restore original codes
               original_codes <- data$old_codes
               for (i in seq_along(indices)) {
                 rv$annotations$code[indices[i]] <- original_codes[i %% length(original_codes) + 1]
               }
             }
             # Restore code colors
             for (old_code in data$old_codes) {
               if (!is.null(data$old_colors[[old_code]])) {
                 rv$code_colors[old_code] <- data$old_colors[[old_code]]
               }
             }
             rv$code_colors <- rv$code_colors[names(rv$code_colors) != data$new_code]
           } else {
             # Store old colors before merge
             old_colors <- list()
             for (code in data$old_codes) {
               old_colors[[code]] <- rv$code_colors[code]
             }

             # Update annotations with new code
             rv$annotations$code[rv$annotations$code %in% data$old_codes] <- data$new_code

             # Update code colors
             rv$code_colors[data$new_code] <- sprintf("#%06X", sample(0:16777215, 1))
             # Remove old code colors
             rv$code_colors <- rv$code_colors[!names(rv$code_colors) %in% data$old_codes]

             # Store old colors in reverse data
             data$old_colors <- old_colors
           }
         })

  invisible(rv)
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
