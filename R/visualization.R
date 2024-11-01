#' Generate code frequency visualization
#'
#' @description
#' Creates a barplot visualization showing the frequency of each code in the annotations.
#' The plot displays codes on the x-axis and their frequency counts on the y-axis.
#'
#' @param annotations Data frame containing text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A recordedplot object containing the code frequency visualization
#'
#' @importFrom graphics par barplot
#' @importFrom grDevices recordPlot
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 10, 20),
#'   end = c(5, 15, 25),
#'   code = c("code1", "code2", "code1")
#' )
#' plot <- generate_code_frequency_plot(annotations)
#' print(plot)
#' }
#'
#' @keywords internal
generate_code_frequency_plot <- function(annotations) {
  code_freq <- table(annotations$code)
  code_freq_sorted <- sort(code_freq, decreasing = TRUE)

  par(mar = c(8, 4, 2, 2))
  barplot(code_freq_sorted,
          main = "Code Frequency",
          xlab = "",
          ylab = "Frequency",
          col = "steelblue",
          las = 2)
  return(recordPlot())
}

#' Generate code co-occurrence statistics and visualization
#'
#' @description
#' Performs a comprehensive analysis of code co-occurrences in the text, including
#' calculation of various similarity metrics and generation of network and heatmap
#' visualizations.
#'
#' @param annotations Data frame containing text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item co_occurrence: Matrix of raw co-occurrence counts
#'     \item jaccard_similarity: Matrix of Jaccard similarity coefficients
#'     \item phi_coefficient: Matrix of Phi coefficients
#'     \item network_plot: Network visualization of code relationships
#'     \item heatmap_plot: Heatmap visualization of code co-occurrences
#'     \item summary: List of summary statistics
#'   }
#'
#' @importFrom graphics par plot points text lines image axis
#' @importFrom grDevices rgb colorRampPalette recordPlot
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 10),
#'   end = c(8, 15, 20),
#'   code = c("code1", "code2", "code1")
#' )
#' results <- generate_code_co_occurrence_analysis(annotations)
#' print(results$summary)
#' }
#'
#' @keywords internal
generate_code_co_occurrence_analysis <- function(annotations) {
  # Get unique codes
  codes <- unique(annotations$code)
  n_codes <- length(codes)

  # Initialize matrices
  co_matrix <- matrix(0, nrow = n_codes, ncol = n_codes,
                      dimnames = list(codes, codes))
  jaccard_matrix <- matrix(0, nrow = n_codes, ncol = n_codes,
                           dimnames = list(codes, codes))
  phi_matrix <- matrix(0, nrow = n_codes, ncol = n_codes,
                       dimnames = list(codes, codes))

  # Create binary occurrence vectors for each code
  code_occurrences <- lapply(codes, function(code) {
    intervals <- annotations[annotations$code == code, c("start", "end")]
    sort(unique(unlist(apply(intervals, 1, function(x) seq(x[1], x[2])))))
  })
  names(code_occurrences) <- codes

  # Calculate co-occurrence and similarity matrices
  for (i in 1:n_codes) {
    for (j in 1:n_codes) {
      if (i != j) {
        # Get occurrence vectors
        code1_pos <- code_occurrences[[i]]
        code2_pos <- code_occurrences[[j]]

        # Calculate co-occurrence (overlap)
        overlap <- length(intersect(code1_pos, code2_pos))
        co_matrix[i, j] <- overlap

        # Calculate Jaccard similarity
        union_size <- length(unique(c(code1_pos, code2_pos)))
        jaccard_matrix[i, j] <- if(union_size > 0) overlap / union_size else 0

        # Calculate Phi coefficient
        n11 <- overlap  # co-occurrence count
        n00 <- nchar(max(annotations$end)) - length(unique(c(code1_pos, code2_pos)))  # neither code
        n10 <- length(code1_pos) - overlap  # code1 only
        n01 <- length(code2_pos) - overlap  # code2 only
        n <- n11 + n10 + n01 + n00

        # Phi coefficient calculation
        if (n > 0) {
          expected <- (length(code1_pos) * length(code2_pos)) / n
          phi_matrix[i, j] <- (n11 - expected) /
            sqrt((length(code1_pos) * length(code2_pos) *
                    (n - length(code1_pos)) * (n - length(code2_pos))) / n)
        }
      }
    }
  }

  # Generate network visualization
  network_plot <- function() {
    if (n_codes <= 1) {
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
           main = if(n_codes == 0) "No codes available" else "Only one code present")
      if (n_codes == 1) {
        points(0.5, 0.5, pch = 16, col = "red", cex = 2)
        text(0.5, 0.5, labels = codes[1], pos = 3, offset = 0.5)
      }
      return(recordPlot())
    }

    # Calculate node positions using circular layout
    angles <- seq(0, 2 * pi, length.out = n_codes + 1)[-1]
    x <- 0.5 + 0.4 * cos(angles)
    y <- 0.5 + 0.4 * sin(angles)

    # Set up plot
    par(mar = c(1, 1, 2, 1))
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
         main = "Code Co-occurrence Network")

    # Draw edges
    for (i in 1:n_codes) {
      for (j in 1:n_codes) {
        if (i < j && jaccard_matrix[i, j] > 0) {
          # Scale line width based on Jaccard similarity
          lwd <- 1 + 5 * jaccard_matrix[i, j]
          # Scale opacity based on phi coefficient
          alpha <- 0.3 + 0.7 * abs(phi_matrix[i, j])
          lines(c(x[i], x[j]), c(y[i], y[j]),
                lwd = lwd,
                col = grDevices::rgb(0, 0, 1, alpha = alpha))
        }
      }
    }

    # Draw nodes and labels
    points(x, y, pch = 16, col = "red", cex = 2)
    text(x, y, labels = codes, pos = 3, offset = 0.5)

    return(grDevices::recordPlot())
  }

  # Heatmap plot function
  heatmap_plot <- function() {
    if (n_codes == 0) {
      plot(1, type = "n", xlab = "", ylab = "",
           main = "No codes available for heatmap")
      return(grDevices::recordPlot())
    }

    # Set up the plotting area
    par(mar = c(8, 8, 2, 2))

    # Create the heatmap
    graphics::image(1:n_codes, 1:n_codes, jaccard_matrix,
                    main = "Code Co-occurrence Heatmap",
                    xlab = "", ylab = "",
                    axes = FALSE,
                    col = grDevices::colorRampPalette(c("white", "steelblue"))(100))

    # Add axes with code labels
    graphics::axis(1, 1:n_codes, codes, las = 2)
    graphics::axis(2, 1:n_codes, codes, las = 2)

    return(grDevices::recordPlot())
  }

  # Return results
  return(list(
    co_occurrence = co_matrix,
    jaccard_similarity = jaccard_matrix,
    phi_coefficient = phi_matrix,
    network_plot = network_plot(),
    heatmap_plot = heatmap_plot(),
    summary = list(
      total_codes = n_codes,
      max_co_occurrence = max(co_matrix),
      max_jaccard = max(jaccard_matrix),
      mean_jaccard = mean(jaccard_matrix[upper.tri(jaccard_matrix)]),
      significant_pairs = sum(abs(phi_matrix) > 0.3, na.rm = TRUE) / 2
    )
  ))
}

#' Generate word cloud visualization
#'
#' @description
#' Creates a simple word cloud visualization from the input text, showing the most
#' frequent words with size proportional to their frequency.
#'
#' @param text Character string containing the text to visualize
#'
#' @return A plot object containing the word cloud visualization
#'
#' @importFrom graphics plot text
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' text <- "This is a sample text with repeated words. Sample text for visualization."
#' cloud <- generate_word_cloud(text)
#' }
generate_word_cloud <- function(text) {
  words <- unlist(strsplit(tolower(text), "\\W+"))
  word_freq <- sort(table(words[nchar(words) > 3]), decreasing = TRUE)
  word_freq <- word_freq[1:min(100, length(word_freq))]

  plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1),
       main = "Word Cloud")

  n <- length(word_freq)
  angles <- runif(n, 0, 2 * pi)
  x <- 0.5 + 0.4 * cos(angles)
  y <- 0.5 + 0.4 * sin(angles)

  sizes <- 1 + 3 * (word_freq - min(word_freq)) / (max(word_freq) - min(word_freq))
  text(x, y, labels = names(word_freq), cex = sizes,
       col = rainbow(n, s = 0.7, v = 0.7))

  return(recordPlot())
}

#' Generate comparison visualizations
#'
#' @description
#' Creates a set of visualizations for comparing coding patterns between different
#' coders, including distribution comparisons, overlap patterns, and sequence patterns.
#'
#' @param comparison_results List containing results from generate_comparison_analysis:
#'   \itemize{
#'     \item coding_strategies: List of analyzed coding patterns
#'     \item comparison: List of comparative analyses
#'   }
#'
#' @return List containing plot objects:
#'   \itemize{
#'     \item distribution: Plot comparing code distribution patterns
#'     \item overlap: Plot showing code overlap patterns
#'     \item sequence: Plot displaying code sequence patterns
#'   }
#'
#' @importFrom graphics par barplot text title
#' @importFrom grDevices recordPlot
#'
#' @examples
#' \dontrun{
#' # Create sample comparison results
#' results <- generate_comparison_analysis(list(
#'   annotations1 = data.frame(start = c(1, 5), end = c(3, 8), code = c("A", "B")),
#'   annotations2 = data.frame(start = c(2, 6), end = c(4, 9), code = c("A", "C"))
#' ))
#'
#' # Generate visualizations
#' plots <- generate_comparison_plots(results)
#' print(plots$distribution)
#' }
#'
#' @keywords internal
generate_comparison_plots <- function(comparison_results) {
  if (is.null(comparison_results) || length(comparison_results$coding_strategies) < 2) {
    return(list(
      distribution = NULL,
      overlap = NULL,
      sequence = NULL
    ))
  }

  # Distribution comparison plot
  distribution_plot <- function() {
    # Calculate total height needed
    n_plots <- length(comparison_results$coding_strategies)

    # Set up the plotting area with adjusted margins
    par(mfrow = c(n_plots, 1),
        # Adjust margins (bottom, left, top, right) to be smaller
        mar = c(3, 2, 2, 1),
        # Add outer margins (bottom, left, top, right)
        oma = c(2, 2, 1, 1))

    for (i in seq_along(comparison_results$coding_strategies)) {
      strategy <- comparison_results$coding_strategies[[i]]
      if (!is.null(strategy$coverage$distribution$frequencies)) {
        freqs <- strategy$coverage$distribution$frequencies
        bp <- barplot(freqs,
                      main = paste("Coder", i),
                      las = 2,           # Rotate labels
                      cex.names = 0.7,   # Reduce label size
                      cex.axis = 0.7,    # Reduce axis text size
                      col = "steelblue",
                      ylim = c(0, max(freqs) * 1.2)) # Add some space at top
        # Add value labels on top of bars
        text(x = bp, y = freqs, labels = freqs, pos = 3, cex = 0.6)
      }
    }
    title(main = "Code Distribution Comparison",
          outer = TRUE,
          line = 0)
    recordPlot()
  }

  # Code overlap patterns plot
  overlap_plot <- function() {
    n_plots <- length(comparison_results$coding_strategies)

    par(mfrow = c(n_plots, 1),
        mar = c(3, 2, 2, 1),
        oma = c(2, 2, 1, 1))

    for (i in seq_along(comparison_results$coding_strategies)) {
      strategy <- comparison_results$coding_strategies[[i]]
      if (!is.null(strategy$co_occurrences$combinations$frequencies)) {
        freqs <- strategy$co_occurrences$combinations$frequencies
        if (length(freqs) > 0) {
          bp <- barplot(freqs,
                        main = paste("Coder", i),
                        las = 2,
                        cex.names = 0.7,
                        cex.axis = 0.7,
                        col = "lightgreen",
                        ylim = c(0, max(freqs) * 1.2))
          text(x = bp, y = freqs, labels = freqs, pos = 3, cex = 0.6)
        } else {
          plot.new()
          title(main = paste("Coder", i, "- No code co-occurrences"))
        }
      }
    }
    title(main = "Code Co-occurrence Comparison",
          outer = TRUE,
          line = 0)
    recordPlot()
  }

  # Sequence patterns plot
  sequence_plot <- function() {
    n_plots <- length(comparison_results$coding_strategies)

    par(mfrow = c(n_plots, 1),
        mar = c(3, 2, 2, 1),
        oma = c(2, 2, 1, 1))

    for (i in seq_along(comparison_results$coding_strategies)) {
      strategy <- comparison_results$coding_strategies[[i]]
      if (!is.null(strategy$sequences$transitions) &&
          length(strategy$sequences$transitions) > 0) {
        # Convert transitions to table
        trans_table <- table(sapply(strategy$sequences$transitions,
                                    function(x) paste(x["from"], "->", x["to"])))
        if (length(trans_table) > 0) {
          bp <- barplot(trans_table,
                        main = paste("Coder", i),
                        las = 2,
                        cex.names = 0.7,
                        cex.axis = 0.7,
                        col = "salmon",
                        ylim = c(0, max(trans_table) * 1.2))
          text(x = bp, y = trans_table, labels = trans_table, pos = 3, cex = 0.6)
        } else {
          plot.new()
          title(main = paste("Coder", i, "- No code sequences"))
        }
      }
    }
    title(main = "Code Sequence Comparison",
          outer = TRUE,
          line = 0)
    recordPlot()
  }

  # Generate and return all plots
  tryCatch({
    list(
      distribution = distribution_plot(),
      overlap = overlap_plot(),
      sequence = sequence_plot()
    )
  }, error = function(e) {
    warning("Error generating plots: ", e$message)
    list(
      distribution = NULL,
      overlap = NULL,
      sequence = NULL
    )
  })
}


#' Plot code distribution visualization
#'
#' @description
#' Creates a barplot visualization showing the distribution of codes in the annotations.
#' The plot includes rotated labels for better readability and handles empty or NULL
#' input data gracefully.
#'
#' @param distribution List containing code distribution information:
#'   \itemize{
#'     \item frequencies: Named numeric vector containing code frequencies
#'   }
#' @param main Character string specifying the plot title
#' @param ... Additional arguments passed to barplot()
#'
#' @return Invisible NULL, called for side effect of creating plot
#'
#' @importFrom graphics barplot text par
#' @importFrom grDevices recordPlot
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Create sample distribution data
#' dist <- list(
#'   frequencies = c(
#'     "Code1" = 5,
#'     "Code2" = 3,
#'     "Code3" = 7
#'   )
#' )
#'
#' # Create basic plot
#' plot_code_distribution(dist)
#'
#' # Create plot with custom title
#' plot_code_distribution(dist, main = "Code Distribution")
#'
#' # Handle empty distribution
#' plot_code_distribution(list(frequencies = integer(0)))
#' }
plot_code_distribution <- function(distribution, main = "", ...) {
  if (is.null(distribution) || length(distribution$frequencies) == 0) {
    plot(0, 0, type = "n",
         main = main,
         xlab = "No distribution data available",
         ylab = "")
    return()
  }

  # Create barplot with rotated labels
  bp <- barplot(distribution$frequencies,
                main = main,
                xlab = "",
                ylab = "Frequency",
                las = 2,
                cex.names = 0.8,
                ...)

  # Add labels below
  text(x = bp,
       y = par("usr")[3] - 0.1,
       labels = names(distribution$frequencies),
       xpd = TRUE,
       srt = 45,
       adj = 1,
       cex = 0.7)
}

#' Plot code overlap patterns
#'
#' @description
#' Creates a barplot visualization of code overlap patterns, showing the frequency
#' of different code co-occurrences with rotated labels for better readability.
#'
#' @param overlaps List containing overlap information:
#'   \itemize{
#'     \item combinations: List containing frequencies of code co-occurrences
#'   }
#' @param main Character string for plot title
#' @param ... Additional arguments passed to barplot()
#'
#' @return Invisible NULL, called for side effect of creating plot
#'
#' @importFrom graphics barplot text par
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' overlaps <- list(
#'   combinations = list(
#'     frequencies = c("A-B" = 2, "B-C" = 3)
#'   )
#' )
#' plot_overlap_patterns(overlaps, main = "Code Overlaps")
#' }
plot_overlap_patterns <- function(overlaps, main = "", ...) {
  if (is.null(overlaps) || length(overlaps$combinations$frequencies) == 0) {
    plot(0, 0, type = "n",
         main = main,
         xlab = "No overlaps available",
         ylab = "")
    return()
  }

  # Create barplot with rotated labels
  bp <- barplot(overlaps$combinations$frequencies,
                main = main,
                xlab = "",
                ylab = "Overlap Count",
                las = 2,
                cex.names = 0.8,
                ...)

  # Add labels below
  text(x = bp,
       y = par("usr")[3] - 0.1,
       labels = names(overlaps$combinations$frequencies),
       xpd = TRUE,
       srt = 45,
       adj = 1,
       cex = 0.7)
}

#' Plot code sequence patterns
#'
#' @description
#' Creates a barplot visualization of code sequence patterns, showing the frequency
#' of different code transitions with rotated labels for better readability.
#'
#' @param sequences List containing sequence information:
#'   \itemize{
#'     \item transitions: List of code transitions
#'   }
#' @param main Character string for plot title
#' @param ... Additional arguments passed to barplot()
#'
#' @return Invisible NULL, called for side effect of creating plot
#'
#' @importFrom graphics barplot text par
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' sequences <- list(
#'   transitions = list(
#'     c(from = "A", to = "B"),
#'     c(from = "B", to = "C")
#'   )
#' )
#' plot_sequence_patterns(sequences, main = "Code Sequences")
#' }
plot_sequence_patterns <- function(sequences, main = "", ...) {
  if (is.null(sequences) || length(sequences$transitions) == 0) {
    plot(0, 0, type = "n",
         main = main,
         xlab = "No sequences available",
         ylab = "")
    return()
  }

  # Convert transitions to table
  trans_table <- table(sapply(sequences$transitions,
                              function(x) paste(x[1], "->", x[2])))

  # Create barplot with adjusted margins
  bp <- barplot(trans_table,
                main = main,
                xlab = "",
                ylab = "Frequency",
                las = 2,
                cex.names = 0.8,
                ...)

  # Add labels below
  text(x = bp,
       y = par("usr")[3] - 0.1,
       labels = names(trans_table),
       xpd = TRUE,
       srt = 45,
       adj = 1,
       cex = 0.7)
}

