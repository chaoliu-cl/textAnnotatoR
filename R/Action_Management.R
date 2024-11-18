#' Create Action for History Tracking
#'
#' @param type Type of action (character string: "add_annotation", "delete_annotation", or "merge_codes")
#' @param data List containing action data
#' @param reverse_data List containing data for reversing the action
#' @return List containing action details with elements:
#'   \itemize{
#'     \item type: Character string specifying action type
#'     \item data: List of action data
#'     \item reverse_data: List of data for reversing action
#'     \item timestamp: POSIXct timestamp of action creation
#'   }
#' @examples
#' action <- create_action(
#'   type = "add_annotation",
#'   data = list(
#'     start = 1,
#'     end = 10,
#'     text = "Sample text",
#'     code = "CODE1"
#'   )
#' )
#' @keywords internal
create_action <- function(type, data, reverse_data = NULL) {
  # Validate input parameters
  valid_types <- c("add_annotation", "delete_annotation", "merge_codes")
  if (!is.character(type) || length(type) != 1 || !(type %in% valid_types)) {
    stop("'type' must be one of: ", paste(valid_types, collapse = ", "))
  }
  if (!is.list(data)) {
    stop("'data' must be a list")
  }
  if (!is.null(reverse_data) && !is.list(reverse_data)) {
    stop("'reverse_data' must be NULL or a list")
  }

  list(
    type = type,
    data = data,
    reverse_data = reverse_data,
    timestamp = Sys.time()
  )
}

#' Add Action to History
#'
#' @param rv Reactive values object containing:
#'   \itemize{
#'     \item action_history: List of actions
#'     \item action_index: Numeric index of current action
#'   }
#' @param action List created by create_action()
#' @return None (modifies reactive values object)
#' @examples
#' \dontrun{
#' rv <- reactiveValues(
#'   action_history = list(),
#'   action_index = 0
#' )
#' action <- create_action("add_annotation", list(start = 1, end = 10))
#' add_action(rv, action)
#' }
#' @keywords internal
add_action <- function(rv, action) {
  # Validate input parameters
  if (!is.list(rv) || is.null(rv$action_history) || is.null(rv$action_index)) {
    stop("Invalid reactive values object")
  }
  if (!is.list(action) || is.null(action$type) || is.null(action$data)) {
    stop("Invalid action object")
  }

  # Remove any future actions if we're not at the end
  if (rv$action_index < length(rv$action_history)) {
    rv$action_history <- rv$action_history[1:rv$action_index]
  }

  # Add the new action
  rv$action_history[[rv$action_index + 1]] <- action
  rv$action_index <- rv$action_index + 1
}


#' Apply Action to Current State
#'
#' Applies or reverses an action on the current state of annotations and codes.
#'
#' @param rv Reactive values object containing:
#'   \itemize{
#'     \item annotations: Data frame of current annotations
#'     \item merged_codes: List of merged code mappings
#'   }
#' @param action Action object containing:
#'   \itemize{
#'     \item type: Type of action to apply
#'     \item data: Action-specific data
#'     \item reverse_data: Data for reversing the action
#'   }
#' @param reverse Boolean indicating if action should be reversed (for undo)
#' @return None (modifies reactive values object in place)
#' @examples
#' \dontrun{
#' # Initialize reactive values
#' rv <- shiny::reactiveValues(
#'   annotations = data.frame(
#'     start = numeric(0),
#'     end = numeric(0),
#'     code = character(0),
#'     stringsAsFactors = FALSE
#'   ),
#'   merged_codes = list()
#' )
#'
#' # Create and apply an action
#' action <- create_action(
#'   type = "add_annotation",
#'   data = list(
#'     start = 1,
#'     end = 10,
#'     text = "Sample text",
#'     code = "CODE1"
#'   )
#' )
#' apply_action(rv, action)
#'
#' # Reverse the action
#' apply_action(rv, action, reverse = TRUE)
#' }
#' @keywords internal
apply_action <- function(rv, action, reverse = FALSE) {
  data <- if (reverse) action$reverse_data else action$data

  switch(action$type,
         "add_annotation" = {
           if (reverse) {
             # Remove annotation
             rv$annotations <- rv$annotations[-which(
               rv$annotations$start == data$start &
                 rv$annotations$end == data$end &
                 rv$annotations$code == data$code
             ), ]
           } else {
             # Add annotation
             rv$annotations <- rbind(rv$annotations, data)
           }
         },
         "delete_annotation" = {
           if (reverse) {
             # Restore annotation
             rv$annotations <- rbind(rv$annotations, data)
           } else {
             # Delete annotation
             rv$annotations <- rv$annotations[-which(
               rv$annotations$start == data$start &
                 rv$annotations$end == data$end &
                 rv$annotations$code == data$code
             ), ]
           }
         },
         "merge_codes" = {
           if (reverse) {
             # Unmerge codes
             rv$annotations$code[rv$annotations$code == data$new_code] <-
               rv$merged_codes[[data$new_code]]
             rv$merged_codes[[data$new_code]] <- NULL
           } else {
             # Merge codes
             rv$merged_codes[[data$new_code]] <- data$old_codes
             rv$annotations$code[rv$annotations$code %in% data$old_codes] <-
               data$new_code
           }
         }
  )
}
