#' Analyze coverage patterns in annotations
#'
#' @description
#' Analyzes how codes are distributed throughout the text, including clustering
#' patterns and coding density.
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
#'     \item clusters: List of annotation clusters
#'     \item density: List containing overall density metrics
#'     \item distribution: List containing code frequencies and positions
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 20),
#'   end = c(3, 8, 25),
#'   code = c("code1", "code2", "code1")
#' )
#' results <- analyze_coverage(annotations)
#' }
#'
#' @keywords internal
analyze_coverage <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) == 0) {
    return(list(
      clusters = list(),
      density = list(overall_density = 0),
      distribution = list(
        frequencies = integer(0),
        positions = list()
      )
    ))
  }

  # Ensure numeric values and handle NAs
  annotations$start <- as.numeric(as.character(annotations$start))
  annotations$end <- as.numeric(as.character(annotations$end))
  annotations$code <- as.character(annotations$code)

  valid_rows <- !is.na(annotations$start) &
    !is.na(annotations$end) &
    !is.na(annotations$code) &
    annotations$start <= annotations$end

  annotations <- annotations[valid_rows, ]

  if (nrow(annotations) == 0) {
    return(list(
      clusters = list(),
      density = list(overall_density = 0),
      distribution = list(
        frequencies = integer(0),
        positions = list()
      )
    ))
  }

  # Sort annotations by position
  sorted_anns <- annotations[order(annotations$start), ]

  # Calculate code frequencies
  code_freq <- table(sorted_anns$code)

  # Calculate code positions with error handling
  code_pos <- tryCatch({
    tapply(sorted_anns$start, sorted_anns$code, function(x) list(positions = x))
  }, error = function(e) {
    list()
  })

  # Calculate density with error handling
  total_length <- max(sorted_anns$end) - min(sorted_anns$start)
  total_coded <- sum(sorted_anns$end - sorted_anns$start + 1)
  density <- if (total_length > 0) total_coded / total_length else 0

  return(list(
    clusters = find_annotation_clusters(sorted_anns),
    density = list(overall_density = density),
    distribution = list(
      frequencies = code_freq,
      positions = code_pos
    )
  ))
}

#' Analyze code application patterns
#'
#' @description
#' Analyzes patterns in how codes are applied in the annotations.
#'
#' @param annotations Data frame containing code annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position
#'     \item end: numeric, ending position
#'     \item code: character, code identifier
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item patterns: List of code patterns
#'     \item summary: Summary statistics
#'   }
#'
#' @keywords internal
analyze_code_patterns <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) == 0) {
    return(list(
      patterns = list(),
      summary = list(total_codes = 0)
    ))
  }

  # Group annotations by code with error handling
  code_groups <- tryCatch({
    split(annotations, annotations$code)
  }, error = function(e) {
    list()
  })

  # Analyze patterns for each code
  code_patterns <- lapply(code_groups, function(code_anns) {
    tryCatch({
      lengths <- code_anns$end - code_anns$start + 1
      list(
        typical_length = mean(lengths, na.rm = TRUE),
        length_variation = stats::sd(lengths, na.rm = TRUE),
        code_count = nrow(code_anns)
      )
    }, error = function(e) {
      list(
        typical_length = 0,
        length_variation = 0,
        code_count = 0
      )
    })
  })

  return(list(
    patterns = code_patterns,
    summary = list(
      total_codes = length(code_patterns),
      unique_codes = length(unique(annotations$code))
    )
  ))
}

#' Analyze code co-occurrence patterns
#'
#' @description
#' Analyzes how different codes co-occur within the annotated text by examining overlapping
#' annotations and calculating various metrics of co-occurrence strength.
#'
#' @param annotations A data frame containing text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A list containing co-occurrence analysis results:
#'   \itemize{
#'     \item combinations: list containing frequency table of code co-occurrences
#'     \item characteristics: list with average overlap length and total overlap count
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 10),
#'   end = c(8, 15, 20),
#'   code = c("code1", "code2", "code1")
#' )
#' results <- analyze_co_occurrences(annotations)
#' }
analyze_co_occurrences <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) <= 1) {
    return(list(
      combinations = list(frequencies = integer(0)),
      characteristics = list(avg_length = 0, total_overlaps = 0)
    ))
  }

  # Find overlapping annotations with error handling
  overlaps <- find_overlapping_codes(annotations)

  # Analyze overlap patterns
  if (length(overlaps) > 0) {
    combinations <- tryCatch({
      table(sapply(overlaps, function(x) {
        if (!is.null(x$code1) && !is.null(x$code2)) {
          paste(sort(c(x$code1, x$code2)), collapse = "-")
        } else {
          NA
        }
      }))
    }, error = function(e) {
      table(character(0))
    })

    lengths <- sapply(overlaps, function(x) {
      if (!is.null(x$overlap_start) && !is.null(x$overlap_end)) {
        x$overlap_end - x$overlap_start + 1
      } else {
        NA
      }
    })

    avg_length <- mean(lengths, na.rm = TRUE)
    if (is.nan(avg_length)) avg_length <- 0
  } else {
    combinations <- table(character(0))
    avg_length <- 0
  }

  return(list(
    combinations = list(frequencies = combinations),
    characteristics = list(
      avg_length = avg_length,
      total_overlaps = length(overlaps)
    )
  ))
}

#' Analyze sequences and transitions between codes
#'
#' @description
#' Analyzes how codes are sequenced in the text by examining transitions
#' between consecutive codes and identifying repeated patterns.
#'
#' @param annotations Data frame of text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item transitions: List of transitions between consecutive codes
#'     \item patterns: List of identified repeated code sequences
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 10, 20),
#'   end = c(5, 15, 25),
#'   code = c("code1", "code2", "code1")
#' )
#' results <- analyze_sequences(annotations)
#' }
#' @keywords internal
analyze_sequences <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) <= 1) {
    return(list(
      transitions = list(),
      patterns = list()
    ))
  }

  # Ensure proper types
  annotations$start <- as.numeric(annotations$start)
  annotations$end <- as.numeric(annotations$end)
  annotations$code <- as.character(annotations$code)

  # Remove any rows with NA values
  valid_rows <- stats::complete.cases(annotations[, c("start", "end", "code")])
  annotations <- annotations[valid_rows, ]

  if (nrow(annotations) <= 1) {
    return(list(
      transitions = list(),
      patterns = list()
    ))
  }

  # Sort annotations by position
  sorted_anns <- annotations[order(annotations$start), ]

  # Find transitions
  transitions <- list()
  for (i in 1:(nrow(sorted_anns)-1)) {
    transitions <- c(transitions,
                     list(c(from = sorted_anns$code[i],
                            to = sorted_anns$code[i+1])))
  }

  return(list(
    transitions = transitions,
    patterns = find_repeated_sequences(sorted_anns)
  ))
}

#' Analyze context around code applications
#'
#' @description
#' Examines the surrounding context where codes are applied by looking at
#' preceding and following annotations to understand code relationships.
#'
#' @param code_anns Data frame containing annotations for specific code:
#'   \itemize{
#'     \item start: numeric, starting position
#'     \item end: numeric, ending position
#'     \item code: character, code identifier
#'   }
#' @param all_anns Data frame containing all annotations in the text
#'
#' @return List of contexts for each code instance:
#'   \itemize{
#'     \item before: Preceding annotation if exists
#'     \item after: Following annotation if exists
#'   }
#'
#' @examples
#' \dontrun{
#' all_annotations <- data.frame(
#'   start = c(1, 10, 20, 30),
#'   end = c(5, 15, 25, 35),
#'   code = c("code1", "code2", "code1", "code3")
#' )
#' code1_anns <- all_annotations[all_annotations$code == "code1", ]
#' contexts <- analyze_code_context(code1_anns, all_annotations)
#' }
#' @keywords internal
analyze_code_context <- function(code_anns, all_anns) {
  if (nrow(code_anns) == 0) return(list())

  contexts <- lapply(1:nrow(code_anns), function(i) {
    current <- code_anns[i, ]

    # Find preceding and following annotations
    preceding <- all_anns[all_anns$end < current$start, ]
    following <- all_anns[all_anns$start > current$end, ]

    # Get closest annotations
    before <- if (nrow(preceding) > 0) {
      preceding[which.max(preceding$end), ]
    } else NULL

    after <- if (nrow(following) > 0) {
      following[which.min(following$start), ]
    } else NULL

    list(
      before = before,
      after = after
    )
  })

  return(contexts)
}

#' Analyze memo usage patterns
#'
#' @description
#' Examines how memos are used with codes by analyzing memo frequency,
#' content, and patterns in memo application across code instances.
#'
#' @param code_anns Data frame containing code annotations with columns:
#'   \itemize{
#'     \item memo: character, memo text associated with annotation
#'     \item code: character, code identifier
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item memo_frequency: Proportion of annotations with memos
#'     \item has_memos: Logical vector indicating memo presence
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   code = c("code1", "code1", "code2"),
#'   memo = c("note 1", "", "note 2")
#' )
#' patterns <- analyze_memo_patterns(annotations)
#' }
#' @keywords internal
analyze_memo_patterns <- function(code_anns) {
  if (nrow(code_anns) == 0) return(list())

  # Extract and analyze memo patterns
  has_memo <- !is.na(code_anns$memo) & code_anns$memo != ""
  memo_count <- sum(has_memo)

  return(list(
    memo_frequency = memo_count / nrow(code_anns),
    has_memos = has_memo
  ))
}

#' Find overlapping code annotations
#'
#' @description
#' Identifies pairs of annotations that overlap in the text and returns their
#' intersection points and associated codes.
#'
#' @param annotations A data frame containing text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A list of overlapping code pairs, each containing:
#'   \itemize{
#'     \item code1: first code in the overlap
#'     \item code2: second code in the overlap
#'     \item overlap_start: starting position of overlap
#'     \item overlap_end: ending position of overlap
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 10),
#'   end = c(8, 15, 20),
#'   code = c("code1", "code2", "code1")
#' )
#' overlaps <- find_overlapping_codes(annotations)
#' }
find_overlapping_codes <- function(annotations) {
  if (!is.data.frame(annotations) || nrow(annotations) <= 1) {
    return(list())
  }

  # Ensure proper types and handle NAs
  annotations$start <- as.numeric(as.character(annotations$start))
  annotations$end <- as.numeric(as.character(annotations$end))
  annotations$code <- as.character(annotations$code)

  valid_rows <- !is.na(annotations$start) &
    !is.na(annotations$end) &
    !is.na(annotations$code) &
    annotations$start <= annotations$end

  annotations <- annotations[valid_rows, ]

  if (nrow(annotations) <= 1) {
    return(list())
  }

  overlaps <- list()
  for (i in 1:(nrow(annotations)-1)) {
    for (j in (i+1):nrow(annotations)) {
      # Check for overlap
      if (annotations$start[i] <= annotations$end[j] &&
          annotations$end[i] >= annotations$start[j]) {
        overlaps <- c(overlaps, list(list(
          code1 = annotations$code[i],
          code2 = annotations$code[j],
          overlap_start = max(annotations$start[i], annotations$start[j]),
          overlap_end = min(annotations$end[i], annotations$end[j])
        )))
      }
    }
  }

  return(overlaps)
}

#' Find clusters of annotations in text
#'
#' @description
#' Identifies clusters of annotations that are close together in the text,
#' helping to identify dense coding regions.
#'
#' @param annotations Data frame containing sorted text annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return List of annotation clusters, where each cluster contains annotations
#'         that are within a specified distance of each other
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 5, 100),
#'   end = c(3, 8, 105),
#'   code = c("code1", "code2", "code3")
#' )
#' clusters <- find_annotation_clusters(annotations)
#' }
#'
#' @keywords internal
find_annotation_clusters <- function(annotations) {
  # Sort annotations by position
  sorted_anns <- annotations[order(annotations$start), ]

  # Find clusters where annotations are close together
  clusters <- list()
  current_cluster <- list()

  for (i in 1:(nrow(sorted_anns) - 1)) {
    if (nrow(sorted_anns) == 0) break

    current <- sorted_anns[i, ]
    next_ann <- sorted_anns[i + 1, ]

    # Add current annotation to cluster
    current_cluster <- append(current_cluster, list(current))

    # If gap to next annotation is large, start new cluster
    if ((next_ann$start - current$end) > 50) {  # Adjust threshold as needed
      if (length(current_cluster) > 0) {
        clusters <- append(clusters, list(current_cluster))
      }
      current_cluster <- list()
    }
  }

  # Add last cluster if exists
  if (length(current_cluster) > 0) {
    clusters <- append(clusters, list(current_cluster))
  }

  return(clusters)
}

#' Find transitions between codes
#'
#' @description
#' Identifies and analyzes transitions between consecutive code applications
#' to understand coding sequence patterns.
#'
#' @param annotations Data frame of sorted annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position
#'     \item end: numeric, ending position
#'     \item code: character, code identifier
#'   }
#'
#' @return List of code transitions, each containing:
#'   \itemize{
#'     \item from: Source code
#'     \item to: Target code
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   start = c(1, 10, 20),
#'   end = c(5, 15, 25),
#'   code = c("code1", "code2", "code3")
#' )
#' transitions <- find_code_transitions(annotations)
#' }
#' @keywords internal
find_code_transitions <- function(annotations) {
  if (nrow(annotations) <= 1) return(list())

  # Find sequences of codes
  transitions <- list()
  for (i in 1:(nrow(annotations)-1)) {
    transitions <- append(transitions,
                          list(c(from = annotations$code[i],
                                 to = annotations$code[i+1])))
  }

  return(transitions)
}

#' Find repeated patterns in code sequences
#'
#' @description
#' Identifies repeating patterns of 2-3 codes in sequence to uncover recurring
#' coding structures.
#'
#' @param annotations Data frame of sorted annotations with columns:
#'   \itemize{
#'     \item code: character, code identifier
#'   }
#'
#' @return Named list of pattern frequencies where:
#'   \itemize{
#'     \item names: Code patterns (e.g. "code1-code2")
#'     \item values: Number of occurrences
#'   }
#'
#' @examples
#' \dontrun{
#' annotations <- data.frame(
#'   code = c("A", "B", "A", "B", "C")
#' )
#' patterns <- find_repeated_sequences(annotations)
#' # Returns list showing A-B pattern occurs twice
#' }
#' @keywords internal
find_repeated_sequences <- function(annotations) {
  if (nrow(annotations) <= 1) return(list())

  # Look for repeated patterns in code sequences
  code_sequence <- annotations$code

  # Look for patterns of length 2-3
  patterns <- list()
  for (len in 2:min(3, length(code_sequence))) {
    for (i in 1:(length(code_sequence) - len + 1)) {
      pattern <- code_sequence[i:(i+len-1)]
      pattern_str <- paste(pattern, collapse = "-")
      patterns[[pattern_str]] <- sum(sapply(
        seq_along(code_sequence),
        function(j) {
          if (j + len - 1 > length(code_sequence)) return(FALSE)
          all(code_sequence[j:(j+len-1)] == pattern)
        }
      ))
    }
  }

  # Remove patterns that only occur once
  patterns <- patterns[patterns > 1]

  return(patterns)
}

#' Generate text summary statistics
#'
#' @description
#' Calculates basic summary statistics for the annotated text, including word counts,
#' character counts, annotation counts, and unique code counts.
#'
#' @param text Character string containing the text being analyzed
#' @param annotations Data frame of annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A list containing summary statistics:
#'   \itemize{
#'     \item total_words: total number of words in the text
#'     \item total_characters: total number of characters
#'     \item total_sentences: number of sentences (approximated by punctuation)
#'     \item total_paragraphs: number of paragraphs (non-empty lines)
#'     \item total_annotations: number of annotations
#'     \item unique_codes: number of unique codes used
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' text <- "This is a sample text.\nIt has two paragraphs."
#' annotations <- data.frame(
#'   start = c(1, 10),
#'   end = c(4, 15),
#'   code = c("code1", "code2")
#' )
#' summary <- generate_text_summary(text, annotations)
#' }
generate_text_summary <- function(text, annotations) {
  # Count paragraphs (sequences separated by blank lines)
  paragraphs <- strsplit(text, "\n")[[1]]
  # Count actual paragraphs (non-empty lines)
  paragraph_count <- sum(nzchar(trimws(paragraphs)))

  list(
    total_words = length(unlist(strsplit(text, "\\W+"))),
    total_characters = nchar(text),
    total_sentences = length(unlist(strsplit(text, "[.!?]+\\s+"))),
    total_paragraphs = paragraph_count,
    total_annotations = nrow(annotations),
    unique_codes = length(unique(annotations$code))
  )
}
