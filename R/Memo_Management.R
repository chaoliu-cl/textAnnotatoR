#' Concatenate Memos
#'
#' Combines two memo texts with proper separation. If the existing memo is empty,
#' returns only the new memo. Otherwise, combines them with a semicolon separator.
#'
#' @param existing_memo Current memo text (character string, can be empty)
#' @param new_memo New memo text to add (character string)
#' @return Combined memo text as a character string
#' @examples
#' # Adding to an empty memo
#' result1 <- concatenate_memos("", "First memo")
#' print(result1)  # "First memo"
#'
#' # Adding to an existing memo
#' result2 <- concatenate_memos("First memo", "Second memo")
#' print(result2)  # "First memo; Second memo"
#'
#' # Multiple additions
#' result3 <- concatenate_memos(result2, "Third memo")
#' print(result3)  # "First memo; Second memo; Third memo"
#' @keywords internal
concatenate_memos <- function(existing_memo, new_memo) {
  if (existing_memo == "") {
    return(new_memo)
  } else {
    return(paste(existing_memo, new_memo, sep = "; "))
  }
}


#' Save Annotations as Text
#'
#' Exports the current annotations to a plain text file. Each annotation is
#' represented with its text content and associated code.
#'
#' @param filename Character string specifying the output file path
#' @return None (writes to file). Invisibly returns TRUE if successful.
#' @examples
#' \dontrun{
#' # Save annotations to a text file
#' filename <- "annotations_export.txt"
#' save_as_text(filename)
#' }
#' @keywords internal
save_as_text <- function(filename) {
  # Get the annotated text
  annotated_text <- create_plain_text_annotations()

  # Write the content to a file
  writeLines(annotated_text, filename)
}


#' Save Annotations as HTML
#'
#' Exports the current annotations to an HTML file with proper formatting and styling.
#' Includes CSS for code highlighting and maintains the visual representation of
#' the annotations.
#'
#' @param filename Character string specifying the output file path
#' @return None (writes to file). Invisibly returns TRUE if successful.
#' @examples
#' \dontrun{
#' # Save annotations to an HTML file
#' filename <- "annotations_export.html"
#' save_as_html(filename)
#' }
#' @keywords internal
save_as_html <- function(filename) {
  # Get the current state of the text display
  html_content <- update_text_display()

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
