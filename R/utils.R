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

#' Generate readable highlight color
#'
#' @description
#' Generates a random color that provides good contrast with black text by ensuring
#' the color is bright enough. Uses HSL color space to maintain good saturation
#' while ensuring sufficient lightness.
#'
#' @return Character string containing a hex color code
#' @keywords internal
generate_readable_color <- function() {
  # Generate random hue (0-360)
  hue <- sample(0:359, 1)

  # Set saturation between 40-70% for vibrant but not overwhelming colors
  saturation <- sample(40:70, 1)

  # Set lightness between 70-90% to ensure readability with black text
  lightness <- sample(70:90, 1)

  # Convert HSL to RGB
  rgb_color <- hsl_to_rgb(hue, saturation, lightness)

  # Return as hex color
  return(sprintf("#%02X%02X%02X", rgb_color[1], rgb_color[2], rgb_color[3]))
}

#' Convert HSL to RGB color values
#'
#' @description
#' Converts HSL (Hue, Saturation, Lightness) color values to RGB values.
#' This allows for better control over color brightness and readability.
#'
#' @param h Numeric, hue value (0-360)
#' @param s Numeric, saturation percentage (0-100)
#' @param l Numeric, lightness percentage (0-100)
#'
#' @return Numeric vector of length 3 containing RGB values (0-255)
#' @keywords internal
hsl_to_rgb <- function(h, s, l) {
  # Normalize values
  h <- h / 360
  s <- s / 100
  l <- l / 100

  if (s == 0) {
    # Achromatic (grey)
    r <- g <- b <- l
  } else {
    hue_to_rgb <- function(p, q, t) {
      if (t < 0) t <- t + 1
      if (t > 1) t <- t - 1
      if (t < 1/6) return(p + (q - p) * 6 * t)
      if (t < 1/2) return(q)
      if (t < 2/3) return(p + (q - p) * (2/3 - t) * 6)
      return(p)
    }

    q <- if (l < 0.5) l * (1 + s) else l + s - l * s
    p <- 2 * l - q

    r <- hue_to_rgb(p, q, h + 1/3)
    g <- hue_to_rgb(p, q, h)
    b <- hue_to_rgb(p, q, h - 1/3)
  }

  # Convert to 0-255 range and round
  return(round(c(r, g, b) * 255))
}

#' Generate predefined readable color palette
#'
#' @description
#' Provides a set of predefined colors that are known to have good readability
#' with black text. This can be used as a fallback or alternative to random generation.
#'
#' @return Character vector of hex color codes
#' @keywords internal
get_readable_color_palette <- function() {
  c(
    "#FFE6CC", "#E6F3FF", "#E6FFE6", "#FFE6F3", "#F3E6FF", "#FFFFE6",
    "#FFE6E6", "#E6F7FF", "#F0FFE6", "#FFE6DC", "#E6E6FF", "#E6FFDC",
    "#FFCCCC", "#CCFFCC", "#CCCCFF", "#FFCCFF", "#CCFFFF", "#FFFFCC",
    "#FFE0CC", "#E0FFCC", "#CCE0FF", "#FFCCE0", "#E0CCFF", "#E0FFE0",
    "#FFEFD5", "#F0FFF0", "#F0F8FF", "#FFF0F5", "#F5F0FF", "#FFFACD",
    "#FFE4E1", "#E0FFFF", "#F5FFFA", "#FFEEE6", "#EEE6FF", "#F0FFF8"
  )
}

#' Get next color from palette with cycling
#'
#' @description
#' Returns the next color from a predefined palette, cycling back to the beginning
#' when all colors have been used. This ensures consistent and readable colors.
#'
#' @param used_colors Character vector of already used colors
#' @return Character string containing a hex color code
#' @keywords internal
get_next_palette_color <- function(used_colors = character(0)) {
  palette <- get_readable_color_palette()

  # Find unused colors
  unused_colors <- setdiff(palette, used_colors)

  if (length(unused_colors) > 0) {
    # Return a random unused color
    return(sample(unused_colors, 1))
  } else {
    # All colors used, cycle back to random palette color
    return(sample(palette, 1))
  }
}

#' Validate color readability
#'
#' @description
#' Checks if a given color provides sufficient contrast with black text.
#' Uses luminance calculation to determine readability.
#'
#' @param color Character string containing hex color code
#' @param min_luminance Numeric, minimum luminance threshold (0-1)
#' @return Logical indicating whether color is readable
#' @keywords internal
is_color_readable <- function(color, min_luminance = 0.6) {
  # Remove # if present
  color <- gsub("#", "", color)

  # Convert hex to RGB
  r <- as.numeric(paste0("0x", substr(color, 1, 2))) / 255
  g <- as.numeric(paste0("0x", substr(color, 3, 4))) / 255
  b <- as.numeric(paste0("0x", substr(color, 5, 6))) / 255

  # Calculate relative luminance
  luminance <- 0.299 * r + 0.587 * g + 0.114 * b

  return(luminance >= min_luminance)
}

#' Update existing dark colors to readable ones
#'
#' @description
#' Checks all existing code colors and replaces any that are too dark
#' with more readable alternatives while trying to maintain color diversity.
#'
#' @param rv ReactiveValues object containing code colors
#' @return Updated ReactiveValues object
#' @keywords internal
update_dark_colors <- function(rv) {
  if (length(rv$code_colors) == 0) return(rv)

  palette <- get_readable_color_palette()
  palette_index <- 1

  for (code in names(rv$code_colors)) {
    current_color <- rv$code_colors[code]

    if (!is_color_readable(current_color)) {
      # Replace with a palette color or generate a new readable one
      if (palette_index <= length(palette)) {
        rv$code_colors[code] <- palette[palette_index]
        palette_index <- palette_index + 1
      } else {
        new_color <- generate_readable_color()
        attempts <- 0
        while (!is_color_readable(new_color) && attempts < 10) {
          new_color <- generate_readable_color()
          attempts <- attempts + 1
        }
        rv$code_colors[code] <- new_color
      }
    }
  }

  return(rv)
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

             # Update code colors - USE NEW READABLE COLOR GENERATION
             used_colors <- as.character(rv$code_colors)
             new_color <- get_next_palette_color(used_colors)

             if (new_color %in% used_colors) {
               new_color <- generate_readable_color()

               attempts <- 0
               while (!is_color_readable(new_color) && attempts < 10) {
                 new_color <- generate_readable_color()
                 attempts <- attempts + 1
               }

               if (!is_color_readable(new_color)) {
                 safe_colors <- c("#FFE6CC", "#E6F3FF", "#E6FFE6", "#FFE6F3", "#F3E6FF")
                 new_color <- sample(safe_colors, 1)
               }
             }

             rv$code_colors[data$new_code] <- new_color
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
