#' Update Text Display
#'
#' @param rv Reactive values object containing:
#'   \itemize{
#'     \item text: Character string of document text
#'     \item annotations: Data frame of annotations
#'     \item code_colors: Named character vector of code colors
#'   }
#' @return HTML string with formatted text and annotations
#' @examples
#' \dontrun{
#' rv <- reactiveValues(
#'   text = "Sample text for annotation",
#'   annotations = data.frame(
#'     start = 1,
#'     end = 6,
#'     code = "Code1",
#'     stringsAsFactors = FALSE
#'   ),
#'   code_colors = c(Code1 = "#FF0000")
#' )
#' html_output <- update_text_display(rv)
#' }
#' @importFrom htmltools HTML
#' @keywords internal
update_text_display <- function() {
  if (nrow(rv$annotations) == 0) {
    return(paste0("<span class='char' id='char_", 1:nchar(rv$text), "'>", strsplit(rv$text, "")[[1]], "</span>", collapse = ""))
  }

  sorted_annotations <- rv$annotations[order(rv$annotations$start), ]
  displayed_text <- ""
  last_end <- 0

  for (i in 1:nrow(sorted_annotations)) {
    if (sorted_annotations$start[i] > last_end + 1) {
      displayed_text <- paste0(displayed_text,
                               paste0("<span class='char' id='char_", (last_end + 1):(sorted_annotations$start[i] - 1), "'>",
                                      strsplit(substr(rv$text, last_end + 1, sorted_annotations$start[i] - 1), "")[[1]],
                                      "</span>", collapse = ""))
    }
    code_color <- rv$code_colors[sorted_annotations$code[i]]
    if (is.null(code_color)) {
      code_color <- "#CCCCCC"  # Default color if not found
    }
    displayed_text <- paste0(displayed_text,
                             "<span class='code-display' style='background-color: ", code_color, ";' data-code='", sorted_annotations$code[i], "' data-start='", sorted_annotations$start[i], "' data-end='", sorted_annotations$end[i], "'>",
                             "[", sorted_annotations$code[i], "]",
                             paste0("<span class='char' id='char_", sorted_annotations$start[i]:sorted_annotations$end[i], "'>",
                                    strsplit(substr(rv$text, sorted_annotations$start[i], sorted_annotations$end[i]), "")[[1]],
                                    "</span>", collapse = ""),
                             "</span>")
    last_end <- sorted_annotations$end[i]
  }

  if (last_end < nchar(rv$text)) {
    displayed_text <- paste0(displayed_text,
                             paste0("<span class='char' id='char_", (last_end + 1):nchar(rv$text), "'>",
                                    strsplit(substr(rv$text, last_end + 1, nchar(rv$text)), "")[[1]],
                                    "</span>", collapse = ""))
  }

  return(displayed_text)
}


#' Create Plain Text Annotations
#'
#' Converts the annotated text into a plain text format where annotations
#' are represented by square brackets containing the code name and annotated text.
#' Annotations are processed in order of their appearance in the text.
#'
#' @param rv Reactive values object containing:
#'   \itemize{
#'     \item text: Character string of document text
#'     \item annotations: Data frame of annotations with columns:
#'       \itemize{
#'         \item start: Numeric vector of start positions
#'         \item end: Numeric vector of end positions
#'         \item code: Character vector of code names
#'       }
#'   }
#' @return Character string containing the text with annotations marked using
#'   square brackets in the format: [code_name: annotated_text]
#' @examples
#' \dontrun{
#' # Initialize reactive values
#' rv <- shiny::reactiveValues(
#'   text = "The quick brown fox jumps over the lazy dog",
#'   annotations = data.frame(
#'     start = c(1, 16, 31),
#'     end = c(8, 19, 34),
#'     code = c("Subject", "Action", "Object"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#'
#' # Convert to plain text format
#' plain_text <- create_plain_text_annotations(rv)
#' # Result: "[Subject: The quick] brown [Action: fox] jumps over the [Object: lazy] dog"
#'
#' # Example with overlapping annotations
#' rv$annotations <- rbind(
#'   rv$annotations,
#'   data.frame(
#'     start = 5,
#'     end = 25,
#'     code = "Description",
#'     stringsAsFactors = FALSE
#'   )
#' )
#' plain_text_complex <- create_plain_text_annotations(rv)
#' }
#' @seealso \code{\link{update_text_display}} for HTML display generation
#' @keywords internal
create_plain_text_annotations <- function() {
  if (nrow(rv$annotations) == 0) {
    return(rv$text)
  }

  sorted_annotations <- rv$annotations[order(rv$annotations$start), ]
  plain_text <- ""
  last_end <- 0

  for (i in 1:nrow(sorted_annotations)) {
    if (sorted_annotations$start[i] > last_end + 1) {
      plain_text <- paste0(plain_text, substr(rv$text, last_end + 1, sorted_annotations$start[i] - 1))
    }
    plain_text <- paste0(plain_text,
                         "[", sorted_annotations$code[i], ": ",
                         substr(rv$text, sorted_annotations$start[i], sorted_annotations$end[i]),
                         "]")
    last_end <- sorted_annotations$end[i]
  }

  if (last_end < nchar(rv$text)) {
    plain_text <- paste0(plain_text, substr(rv$text, last_end + 1, nchar(rv$text)))
  }

  return(plain_text)
}
