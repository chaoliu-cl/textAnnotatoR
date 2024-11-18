#' Generate Code Frequency Plot
#'
#' Creates a bar plot showing the frequency of each code in the annotations.
#'
#' @param annotations Data frame containing columns:
#'   \itemize{
#'     \item code: Factor or character vector of code names
#'   }
#' @return Recorded plot object showing code frequencies
#' @examples
#' annotations <- data.frame(
#'   code = c("Code1", "Code2", "Code1", "Code3", "Code2"),
#'   stringsAsFactors = FALSE
#' )
#' plot <- generate_code_frequency_plot(annotations)
#' @importFrom graphics barplot par
#' @importFrom grDevices recordPlot
#' @export
generate_code_frequency_plot <- function(annotations) {
  # Validate input
  if (!is.data.frame(annotations) || !"code" %in% names(annotations)) {
    stop("annotations must be a data frame with a 'code' column")
  }
  if (nrow(annotations) == 0) {
    warning("No annotations provided for frequency plot")
    return(NULL)
  }

  # Calculate frequencies
  code_freq <- table(annotations$code)
  code_freq_sorted <- sort(code_freq, decreasing = TRUE)

  # Create plot
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mar = c(8, 4, 2, 2))
  barplot(code_freq_sorted,
          main = "Code Frequency",
          xlab = "",
          ylab = "Frequency",
          col = "steelblue",
          las = 2)

  return(recordPlot())
}


#' Generate Code Co-occurrence Network
#'
#' Creates a network visualization showing how different codes co-occur in the text.
#' Codes are represented as nodes, and lines between nodes indicate co-occurrence,
#' with line thickness proportional to co-occurrence frequency.
#'
#' @param annotations Data frame containing columns:
#'   \itemize{
#'     \item code: Factor or character vector of code names
#'     \item [other columns are ignored]
#'   }
#' @return A recorded plot object (from grDevices::recordPlot) showing the
#'         co-occurrence network. If 0 or 1 codes are present, returns a simple
#'         plot with an appropriate message.
#' @examples
#' # Create sample annotations with co-occurring codes
#' annotations <- data.frame(
#'   code = c("Code1", "Code2", "Code1", "Code2", "Code3"),
#'   start = c(1, 1, 10, 10, 20),
#'   end = c(5, 5, 15, 15, 25),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Generate and display the network
#' network <- generate_code_co_occurrence_network(annotations)
#'
#' # Example with single code
#' single_code <- data.frame(
#'   code = "Code1",
#'   start = 1,
#'   end = 5,
#'   stringsAsFactors = FALSE
#' )
#' single_network <- generate_code_co_occurrence_network(single_code)
#' @importFrom graphics plot points text lines
#' @importFrom grDevices recordPlot rgb
#' @export
generate_code_co_occurrence_network <- function(annotations) {
  codes <- unique(annotations$code)

  # Handle case with 0 or 1 code
  if (length(codes) <= 1) {
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         main = if(length(codes) == 0) "No codes available" else "Only one code present")
    if (length(codes) == 1) {
      points(0.5, 0.5, pch = 16, col = "red", cex = 2)
      text(0.5, 0.5, labels = codes[1], pos = 3, offset = 0.5)
    }
    return(recordPlot())
  }

  co_occurrence <- matrix(0, nrow = length(codes), ncol = length(codes),
                          dimnames = list(codes, codes))

  for (i in 1:(nrow(annotations) - 1)) {
    for (j in (i + 1):nrow(annotations)) {
      if (annotations$code[i] != annotations$code[j]) {
        co_occurrence[annotations$code[i], annotations$code[j]] <-
          co_occurrence[annotations$code[i], annotations$code[j]] + 1
        co_occurrence[annotations$code[j], annotations$code[i]] <-
          co_occurrence[annotations$code[j], annotations$code[i]] + 1
      }
    }
  }

  plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
       main = "Code Co-occurrence Network")

  n <- nrow(co_occurrence)
  angles <- seq(0, 2 * pi, length.out = n + 1)[-1]
  x <- 0.5 + 0.4 * cos(angles)
  y <- 0.5 + 0.4 * sin(angles)

  for (i in 1:n) {
    for (j in 1:n) {
      if (co_occurrence[i, j] > 0) {
        lines(c(x[i], x[j]), c(y[i], y[j]),
              lwd = sqrt(co_occurrence[i, j]),
              col = rgb(0, 0, 1, alpha = 0.5))
      }
    }
  }

  points(x, y, pch = 16, col = "red", cex = 2)
  text(x, y, labels = rownames(co_occurrence), pos = 3, offset = 0.5)

  return(recordPlot())
}


#' Generate Word Cloud
#'
#' Creates a word cloud visualization from input text.
#'
#' @param text Character string containing text to analyze
#' @return Recorded plot object showing word cloud
#' @examples
#' text <- "This is a sample text for word cloud generation.
#'          The word cloud will show frequent words in larger size."
#' cloud <- generate_word_cloud(text)
#' @importFrom graphics plot text
#' @importFrom grDevices rainbow recordPlot
#' @export
generate_word_cloud <- function(text) {
  # Validate input
  if (!is.character(text) || length(text) == 0) {
    stop("text must be a non-empty character string")
  }

  # Process text
  words <- unlist(strsplit(tolower(text), "\\W+"))
  if (length(words) == 0) {
    warning("No valid words found in text")
    return(NULL)
  }

  # Calculate word frequencies
  word_freq <- sort(table(words[nchar(words) > 3]), decreasing = TRUE)
  word_freq <- word_freq[1:min(100, length(word_freq))]

  # Create plot
  plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
       main = "Word Cloud")

  n <- length(word_freq)
  if (n == 0) {
    warning("No words to display in cloud")
    return(NULL)
  }

  # Calculate positions and sizes
  angles <- runif(n, 0, 2 * pi)
  x <- 0.5 + 0.4 * cos(angles)
  y <- 0.5 + 0.4 * sin(angles)
  sizes <- 1 + 3 * (word_freq - min(word_freq)) / (max(word_freq) - min(word_freq))

  # Plot words
  text(x, y, labels = names(word_freq), cex = sizes,
       col = rainbow(n, s = 0.7, v = 0.7))

  return(recordPlot())
}

# Analysis_Functions.R (continuation)

#' Generate Text Summary Statistics
#'
#' Calculates various summary statistics from the input text and its annotations,
#' including word count, character count, sentence count, paragraph count,
#' and annotation metrics.
#'
#' @param text Character string of text to analyze. Must be non-empty.
#' @param annotations Data frame containing at minimum:
#'   \itemize{
#'     \item code: Character vector of annotation codes
#'   }
#' @return A list containing the following summary statistics:
#'   \itemize{
#'     \item total_words: Integer count of words in text
#'     \item total_characters: Integer count of characters in text
#'     \item total_sentences: Integer count of sentences (based on punctuation)
#'     \item total_paragraphs: Integer count of paragraphs (based on line breaks)
#'     \item total_annotations: Integer count of annotations
#'     \item unique_codes: Integer count of unique codes used
#'   }
#' @examples
#' # Sample text with multiple sentences and paragraphs
#' text <- "This is the first paragraph. It has two sentences.
#'
#' This is the second paragraph. It also has two sentences."
#'
#' # Sample annotations
#' annotations <- data.frame(
#'   code = c("Introduction", "Conclusion", "Introduction"),
#'   start = c(1, 50, 100),
#'   end = c(45, 95, 145),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Generate summary statistics
#' summary_stats <- generate_text_summary(text, annotations)
#'
#' # Print results
#' print(summary_stats$total_words)  # Number of words
#' print(summary_stats$total_paragraphs)  # Number of paragraphs
#' print(summary_stats$unique_codes)  # Number of unique codes
#' @export
generate_text_summary <- function(text, annotations) {
  list(
    total_words = length(unlist(strsplit(text, "\\W+"))),
    total_characters = nchar(text),
    total_sentences = length(unlist(strsplit(text, "[.!?]+\\s+"))),
    total_paragraphs = length(unlist(strsplit(text, "\n\\s*\n"))),
    total_annotations = nrow(annotations),
    unique_codes = length(unique(annotations$code))
  )
}
