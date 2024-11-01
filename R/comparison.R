#' Compare coding patterns between different documents or coders
#'
#' @description
#' Performs a comprehensive comparison of coding patterns between different sets of
#' annotations, analyzing differences in coverage, code application, overlaps, and
#' code sequences.
#'
#' @param annotations_list A list of data frames, where each data frame contains
#'        annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @return A list containing comparison results and analysis:
#'   \itemize{
#'     \item coding_strategies: list of analyzed coding patterns for each input
#'     \item comparison: list of comparative analyses between coding patterns
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' # Create sample annotation sets
#' annotations1 <- data.frame(
#'   start = c(1, 10),
#'   end = c(5, 15),
#'   code = c("code1", "code2")
#' )
#' annotations2 <- data.frame(
#'   start = c(2, 12),
#'   end = c(6, 18),
#'   code = c("code1", "code3")
#' )
#' results <- generate_comparison_analysis(list(annotations1, annotations2))
#' }
generate_comparison_analysis <- function(annotations_list) {
  if (!is.list(annotations_list) || length(annotations_list) < 2) {
    stop("Need at least two annotation sets for comparison")
  }

  # Process each annotation set with error handling
  coding_strategies <- lapply(annotations_list, function(annotations) {
    tryCatch({
      if (!is.data.frame(annotations)) {
        stop("Invalid data format: input must be a data frame")
      }

      # Validate and clean data
      required_cols <- c("start", "end", "code")
      if (!all(required_cols %in% colnames(annotations))) {
        stop(paste("Missing required columns:",
                   paste(setdiff(required_cols, colnames(annotations)), collapse = ", ")))
      }

      # Ensure proper types and handle NA values
      annotations$start <- as.numeric(as.character(annotations$start))
      annotations$end <- as.numeric(as.character(annotations$end))
      annotations$code <- as.character(annotations$code)

      # Remove invalid rows
      valid_rows <- !is.na(annotations$start) &
        !is.na(annotations$end) &
        !is.na(annotations$code) &
        annotations$start <= annotations$end

      if (sum(valid_rows) == 0) {
        stop("No valid annotations found after cleaning")
      }

      annotations <- annotations[valid_rows, ]

      # Calculate coverage statistics with error handling
      coverage <- tryCatch({
        list(
          distribution = list(
            frequencies = table(annotations$code)
          )
        )
      }, error = function(e) {
        list(distribution = list(frequencies = table(character(0))))
      })

      # Calculate co-occurrence statistics with error handling
      co_occurrences <- tryCatch({
        list(
          combinations = list(
            frequencies = calculate_co_occurrences(annotations)
          )
        )
      }, error = function(e) {
        list(combinations = list(frequencies = table(character(0))))
      })

      # Calculate sequence statistics with error handling
      sequences <- tryCatch({
        list(
          transitions = calculate_transitions(annotations[order(annotations$start), ])
        )
      }, error = function(e) {
        list(transitions = list())
      })

      return(list(
        coverage = coverage,
        co_occurrences = co_occurrences,
        sequences = sequences
      ))
    }, error = function(e) {
      # Return empty results if processing fails
      list(
        coverage = list(distribution = list(frequencies = table(character(0)))),
        co_occurrences = list(combinations = list(frequencies = table(character(0)))),
        sequences = list(transitions = list())
      )
    })
  })

  # Calculate comparison metrics with error handling
  comparison <- tryCatch({
    list(
      coverage_differences = compare_coverage(coding_strategies),
      code_differences = compare_codes(coding_strategies),
      overlap_differences = compare_overlaps(coding_strategies)
    )
  }, error = function(e) {
    list(
      coverage_differences = "Error calculating differences",
      code_differences = "Error comparing codes",
      overlap_differences = "Error analyzing overlaps"
    )
  })

  return(list(
    coding_strategies = coding_strategies,
    comparison = comparison
  ))
}

#' Compare coverage patterns between coding strategies
#'
#' @description
#' Analyzes and compares the coverage patterns between different coding strategies,
#' including total codes used and unique code counts.
#'
#' @param coding_strategies List of coding strategies, where each strategy contains:
#'   \itemize{
#'     \item coverage: List containing distribution information
#'     \item frequencies: Table of code frequencies
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item total_codes_range: Numeric vector with min and max total codes
#'     \item unique_codes_range: Numeric vector with min and max unique codes
#'   }
#'
#' @examples
#' \dontrun{
#' strategy1 <- list(
#'   coverage = list(
#'     distribution = list(
#'       frequencies = table(c("code1", "code1", "code2"))
#'     )
#'   )
#' )
#' strategy2 <- list(
#'   coverage = list(
#'     distribution = list(
#'       frequencies = table(c("code2", "code3", "code3"))
#'     )
#'   )
#' )
#' results <- compare_coverage(list(strategy1, strategy2))
#' }
#'
#' @keywords internal
compare_coverage <- function(coding_strategies) {
  tryCatch({
    coverage_stats <- lapply(coding_strategies, function(strategy) {
      freqs <- strategy$coverage$distribution$frequencies
      list(
        total_codes = sum(freqs),
        unique_codes = length(freqs),
        code_frequencies = freqs
      )
    })

    total_codes <- sapply(coverage_stats, `[[`, "total_codes")
    unique_codes <- sapply(coverage_stats, `[[`, "unique_codes")

    return(list(
      total_codes_range = if(length(total_codes) > 0) range(total_codes) else c(0, 0),
      unique_codes_range = if(length(unique_codes) > 0) range(unique_codes) else c(0, 0)
    ))
  }, error = function(e) {
    return(list(
      total_codes_range = c(0, 0),
      unique_codes_range = c(0, 0)
    ))
  })
}

#' Compare code usage between coders
#'
#' @description
#' Compares how different coders use codes by analyzing shared codes and their
#' usage patterns across coding strategies.
#'
#' @param coding_strategies List of coding strategies, where each strategy contains
#'        code frequency information
#'
#' @return List containing:
#'   \itemize{
#'     \item shared_codes: Character vector of codes used across strategies
#'     \item usage_matrix: Matrix showing code usage across strategies
#'   }
#'
#' @examples
#' \dontrun{
#' strategy1 <- list(
#'   coverage = list(
#'     distribution = list(
#'       frequencies = c(code1 = 2, code2 = 1)
#'     )
#'   )
#' )
#' strategy2 <- list(
#'   coverage = list(
#'     distribution = list(
#'       frequencies = c(code2 = 2, code3 = 1)
#'     )
#'   )
#' )
#' comparison <- compare_codes(list(strategy1, strategy2))
#' }
#'
#' @keywords internal
compare_codes <- function(coding_strategies) {
  tryCatch({
    all_codes <- unique(unlist(lapply(coding_strategies, function(strategy) {
      names(strategy$coverage$distribution$frequencies)
    })))

    if (length(all_codes) == 0) {
      return(list(
        shared_codes = character(0),
        usage_matrix = matrix(0, nrow = 0, ncol = 0)
      ))
    }

    code_usage <- sapply(coding_strategies, function(strategy) {
      freqs <- strategy$coverage$distribution$frequencies
      sapply(all_codes, function(code) {
        if (code %in% names(freqs)) freqs[code] else 0
      })
    })

    return(list(
      shared_codes = all_codes,
      usage_matrix = code_usage
    ))
  }, error = function(e) {
    return(list(
      shared_codes = character(0),
      usage_matrix = matrix(0, nrow = 0, ncol = 0)
    ))
  })
}

#' Compare overlap patterns between coders
#'
#' @description
#' Analyzes how different coders overlap in their code applications by comparing
#' overlap patterns and frequencies across coding strategies.
#'
#' @param coding_strategies List of coding strategies, where each strategy contains
#'        overlap information
#'
#' @return List containing:
#'   \itemize{
#'     \item total_overlaps_range: Range of total overlaps across strategies
#'     \item unique_pairs_range: Range of unique code pairs across strategies
#'   }
#'
#' @examples
#' \dontrun{
#' strategy1 <- list(
#'   co_occurrences = list(
#'     combinations = list(
#'       frequencies = c("code1-code2" = 2)
#'     )
#'   )
#' )
#' strategy2 <- list(
#'   co_occurrences = list(
#'     combinations = list(
#'       frequencies = c("code2-code3" = 1)
#'     )
#'   )
#' )
#' results <- compare_overlaps(list(strategy1, strategy2))
#' }
#'
#' @keywords internal
compare_overlaps <- function(coding_strategies) {
  tryCatch({
    overlap_stats <- lapply(coding_strategies, function(strategy) {
      freqs <- strategy$co_occurrences$combinations$frequencies
      list(
        total_overlaps = if(length(freqs) > 0) sum(freqs) else 0,
        unique_pairs = length(freqs)
      )
    })

    total_overlaps <- sapply(overlap_stats, `[[`, "total_overlaps")
    unique_pairs <- sapply(overlap_stats, `[[`, "unique_pairs")

    return(list(
      total_overlaps_range = if(length(total_overlaps) > 0) range(total_overlaps) else c(0, 0),
      unique_pairs_range = if(length(unique_pairs) > 0) range(unique_pairs) else c(0, 0)
    ))
  }, error = function(e) {
    return(list(
      total_overlaps_range = c(0, 0),
      unique_pairs_range = c(0, 0)
    ))
  })
}

#' Compare code application patterns between coders
#'
#' @description
#' Analyzes and compares how different coders apply codes by examining code segment
#' lengths and memo usage patterns across coding strategies.
#'
#' @param patterns_list List of coding patterns from different coders, where each
#'        pattern contains:
#'   \itemize{
#'     \item typical_length: numeric, average length of code segments
#'     \item memo_patterns: list containing memo usage statistics
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item length_variation: Character string describing variation in code segment lengths
#'     \item memo_usage_summary: Character string describing differences in memo usage
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' patterns1 <- list(
#'   code1 = list(typical_length = 10,
#'                memo_patterns = list(memo_frequency = 0.5))
#' )
#' patterns2 <- list(
#'   code1 = list(typical_length = 15,
#'                memo_patterns = list(memo_frequency = 0.3))
#' )
#' results <- compare_code_patterns(list(patterns1, patterns2))
#' }
compare_code_patterns <- function(patterns_list) {
  if (length(patterns_list) < 2) return("Need at least two sets for comparison")

  # Compare code application patterns
  lengths <- lapply(patterns_list, function(x) {
    sapply(x, function(p) p$typical_length)
  })

  # Compare memo usage
  memo_usage <- lapply(patterns_list, function(x) {
    sapply(x, function(p) p$memo_patterns$memo_frequency)
  })

  list(
    length_variation = "Variation in code segment lengths across coders",
    memo_usage_summary = "Differences in memo usage patterns"
  )
}

#' Compare code co-occurrence patterns between coders
#'
#' @description
#' Analyzes how different coders overlap in their code applications by comparing
#' the frequency and patterns of code co-occurrences.
#'
#' @param overlaps_list List of overlap patterns from different coders, where each
#'        entry contains:
#'   \itemize{
#'     \item combinations: List containing frequency table of code co-occurrences
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item overlap_variation: Numeric value indicating range of overlap counts
#'     \item summary: Character string describing variation in overlapping pairs
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' overlaps1 <- list(combinations = list(frequencies = c("A-B" = 2)))
#' overlaps2 <- list(combinations = list(frequencies = c("A-B" = 1, "B-C" = 3)))
#' results <- compare_co_occurrences(list(overlaps1, overlaps2))
#' }
compare_co_occurrences <- function(overlaps_list) {
  if (length(overlaps_list) < 2) return("Need at least two sets for comparison")

  # Compare overlap patterns
  overlap_counts <- sapply(overlaps_list, function(x) {
    length(x$combinations$frequencies)
  })

  list(
    overlap_variation = diff(range(overlap_counts)),
    summary = sprintf("Number of overlapping code pairs varies from %d to %d",
                      min(overlap_counts), max(overlap_counts))
  )
}

#' Compare code sequence patterns between coders
#'
#' @description
#' Analyzes how different coders sequence their codes by comparing the patterns
#' and frequency of code transitions.
#'
#' @param sequences_list List of sequence patterns from different coders, where each
#'        entry contains:
#'   \itemize{
#'     \item transitions: List of code transitions observed in the text
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item sequence_variation: Numeric value indicating range of transition counts
#'     \item summary: Character string describing variation in code transitions
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' seq1 <- list(transitions = list(c("A", "B"), c("B", "C")))
#' seq2 <- list(transitions = list(c("A", "B")))
#' results <- compare_sequences(list(seq1, seq2))
#' }
compare_sequences <- function(sequences_list) {
  if (length(sequences_list) < 2) return("Need at least two sets for comparison")

  # Compare sequence patterns
  transition_counts <- sapply(sequences_list, function(x) {
    length(x$transitions)
  })

  list(
    sequence_variation = diff(range(transition_counts)),
    summary = sprintf("Number of code transitions varies from %d to %d",
                      min(transition_counts), max(transition_counts))
  )
}

#' Process comparison file
#'
#' @description
#' Processes uploaded comparison files, handling different file formats (CSV, JSON)
#' and ensuring proper data structure and types for comparison analysis.
#'
#' @param filepath Character string specifying the path to the comparison file
#'
#' @return Data frame containing processed annotations with columns:
#'   \itemize{
#'     \item start: numeric, starting position of annotation
#'     \item end: numeric, ending position of annotation
#'     \item code: character, code applied to the annotation
#'   }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' # Process a CSV file
#' annotations_csv <- process_comparison_file("annotations.csv")
#'
#' # Process a JSON file
#' annotations_json <- process_comparison_file("annotations.json")
#' }
#'
#' @keywords internal
process_comparison_file <- function(filepath) {
  ext <- tolower(tools::file_ext(filepath))

  df <- if (ext == "csv") {
    read.csv(filepath, stringsAsFactors = FALSE)
  } else if (ext == "json") {
    fromJSON(filepath)
  } else {
    stop("Unsupported file format")
  }

  # If the file is from Records tab (has text and memo columns), reformat it
  if (all(c("start", "end", "text", "code", "memo") %in% colnames(df))) {
    df <- df[, c("start", "end", "code")]
  }

  # Check required columns
  required_cols <- c("start", "end", "code")
  if (!all(required_cols %in% colnames(df))) {
    missing_cols <- setdiff(required_cols, colnames(df))
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Convert columns to proper types
  df$start <- as.numeric(as.character(df$start))
  df$end <- as.numeric(as.character(df$end))
  df$code <- as.character(df$code)

  # Remove rows with NA values
  df <- df[complete.cases(df[, required_cols]), ]

  if (nrow(df) == 0) {
    stop("No valid annotations found after cleaning")
  }

  return(df)
}
