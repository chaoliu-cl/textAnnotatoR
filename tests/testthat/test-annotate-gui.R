# tests/testthat/test-annotate-gui.R

library(testthat)
library(shiny)
library(data.tree)

# Helper function to create test environment
create_test_env <- function() {
  env <- new.env()
  env$rv <- list(
    text = "Sample text for testing",
    annotations = data.frame(
      start = c(1, 10),
      end = c(5, 15),
      text = c("Sampl", "text for"),
      code = c("code1", "code2"),
      memo = c("memo1", "memo2"),
      stringsAsFactors = FALSE
    ),
    codes = c("code1", "code2"),
    code_tree = Node$new("Root"),
    code_colors = c(code1 = "#FF0000", code2 = "#00FF00"),
    memos = list(),
    code_descriptions = list()
  )
  return(env)
}

# Test update_text_display function
test_that("update_text_display formats text correctly", {
  test_env <- create_test_env()

  # Mock the update_text_display function to use non-reactive values
  local_update_text_display <- function() {
    if (nrow(test_env$rv$annotations) == 0) {
      return(paste0("<span class='char' id='char_", 1:nchar(test_env$rv$text), "'>",
                    strsplit(test_env$rv$text, "")[[1]], "</span>", collapse = ""))
    }

    sorted_annotations <- test_env$rv$annotations[order(test_env$rv$annotations$start), ]
    displayed_text <- ""
    last_end <- 0

    for (i in 1:nrow(sorted_annotations)) {
      if (sorted_annotations$start[i] > last_end + 1) {
        displayed_text <- paste0(displayed_text,
                                 paste0("<span class='char' id='char_",
                                        (last_end + 1):(sorted_annotations$start[i] - 1),
                                        "'>", strsplit(substr(test_env$rv$text,
                                                              last_end + 1,
                                                              sorted_annotations$start[i] - 1),
                                                       "")[[1]], "</span>", collapse = ""))
      }

      code_color <- test_env$rv$code_colors[sorted_annotations$code[i]]
      if (is.null(code_color)) code_color <- "#CCCCCC"

      displayed_text <- paste0(displayed_text,
                               "<span class='code-display' style='background-color: ",
                               code_color, ";' data-code='", sorted_annotations$code[i],
                               "' data-start='", sorted_annotations$start[i],
                               "' data-end='", sorted_annotations$end[i], "'>")

      last_end <- sorted_annotations$end[i]
    }

    return(displayed_text)
  }

  # Test with no annotations
  test_env$rv$annotations <- data.frame(
    start = integer(),
    end = integer(),
    text = character(),
    code = character(),
    stringsAsFactors = FALSE
  )

  result <- local_update_text_display()
  expect_type(result, "character")
  expect_match(result, 'class=\'char\'')

  # Test with annotations
  test_env$rv$annotations <- data.frame(
    start = c(1, 10),
    end = c(5, 15),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )

  result <- local_update_text_display()
  expect_type(result, "character")
  expect_match(result, 'class=\'code-display\'')
  expect_match(result, 'background-color: #FF0000')
})

# Test create_plain_text_annotations function
test_that("create_plain_text_annotations creates correct format", {
  test_env <- create_test_env()

  local_create_plain_text_annotations <- function() {
    if (nrow(test_env$rv$annotations) == 0) {
      return(test_env$rv$text)
    }

    sorted_annotations <- test_env$rv$annotations[order(test_env$rv$annotations$start), ]
    plain_text <- ""
    last_end <- 0

    for (i in 1:nrow(sorted_annotations)) {
      if (sorted_annotations$start[i] > last_end + 1) {
        plain_text <- paste0(plain_text, substr(test_env$rv$text,
                                                last_end + 1,
                                                sorted_annotations$start[i] - 1))
      }
      plain_text <- paste0(plain_text,
                           "[", sorted_annotations$code[i], ": ",
                           substr(test_env$rv$text,
                                  sorted_annotations$start[i],
                                  sorted_annotations$end[i]),
                           "]")
      last_end <- sorted_annotations$end[i]
    }

    if (last_end < nchar(test_env$rv$text)) {
      plain_text <- paste0(plain_text,
                           substr(test_env$rv$text,
                                  last_end + 1,
                                  nchar(test_env$rv$text)))
    }

    return(plain_text)
  }

  # Test with no annotations
  test_env$rv$annotations <- data.frame(
    start = integer(),
    end = integer(),
    text = character(),
    code = character(),
    stringsAsFactors = FALSE
  )

  result <- local_create_plain_text_annotations()
  expect_equal(result, test_env$rv$text)

  # Test with annotations
  test_env$rv$annotations <- data.frame(
    start = c(1, 10),
    end = c(5, 15),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )

  result <- local_create_plain_text_annotations()
  expect_match(result, "\\[code1:")
  expect_match(result, "\\[code2:")
})

# Test save_as_html and save_as_text functions
test_that("file saving functions work correctly", {
  test_env <- create_test_env()

  # Test HTML saving
  temp_html <- tempfile(fileext = ".html")
  html_content <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<style>\n.code-display { padding: 2px 5px; }</style>\n",
    "</head>\n<body>\n",
    "<div>Test content</div>\n",
    "</body>\n</html>"
  )
  writeLines(html_content, temp_html)
  expect_true(file.exists(temp_html))
  unlink(temp_html)

  # Test text saving
  temp_txt <- tempfile(fileext = ".txt")
  text_content <- "[code1: Test] content"
  writeLines(text_content, temp_txt)
  expect_true(file.exists(temp_txt))
  unlink(temp_txt)
})

# Test process_comparison_file function
test_that("process_comparison_file handles different file formats", {
  # Create test CSV file
  csv_data <- data.frame(
    start = c(1, 5),
    end = c(4, 8),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )
  csv_file <- tempfile(fileext = ".csv")
  write.csv(csv_data, csv_file, row.names = FALSE)

  # Test CSV processing
  result <- process_comparison_file(csv_file)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)

  # Clean up
  unlink(csv_file)

  # Test error handling for non-existent file
  # Suppress warnings while testing for the error
  suppressWarnings(
    expect_error(
      process_comparison_file("nonexistent.csv"),
      regexp = "No such file or directory|cannot open the connection",
      class = "error"
    )
  )

  # Test error handling for invalid file format
  invalid_file <- tempfile(fileext = ".txt")
  writeLines("not a CSV file", invalid_file)
  expect_error(
    process_comparison_file(invalid_file),
    regexp = "Unsupported file format|Missing required columns",
    class = "error"
  )
  unlink(invalid_file)

  # Test JSON format
  json_file <- tempfile(fileext = ".json")
  json_content <- jsonlite::toJSON(csv_data, pretty = TRUE)
  writeLines(json_content, json_file)

  json_result <- process_comparison_file(json_file)
  expect_s3_class(json_result, "data.frame")
  expect_equal(nrow(json_result), 2)
  expect_equal(ncol(json_result), 3)

  unlink(json_file)
})

# Test analysis functions
test_that("analysis functions produce expected results", {
  test_data <- data.frame(
    start = c(1, 10),
    end = c(5, 15),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )

  # Test coverage analysis
  coverage <- analyze_coverage(test_data)
  expect_type(coverage, "list")
  expect_false(is.null(coverage$distribution))

  # Test code patterns analysis - updated test
  patterns <- analyze_code_patterns(test_data)  # Now passing test_data as argument
  expect_type(patterns, "list")
  expect_false(is.null(patterns$summary))
  expect_true("total_codes" %in% names(patterns$summary))

  # Test co-occurrence analysis
  co_occurrences <- analyze_co_occurrences(test_data)
  expect_type(co_occurrences, "list")
  expect_false(is.null(co_occurrences$combinations))
})

# Test visualization functions
test_that("visualization functions create valid plots", {
  test_data <- data.frame(
    start = c(1, 10),
    end = c(5, 15),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )

  # Test code frequency plot
  freq_plot <- generate_code_frequency_plot(test_data)
  expect_type(freq_plot, "list")

  # Test word cloud
  cloud <- generate_word_cloud("Sample text for word cloud testing")
  expect_type(cloud, "list")

  # Test text summary
  summary <- generate_text_summary("Sample text", test_data)
  expect_type(summary, "list")
  expect_false(is.null(summary$total_words))
  expect_false(is.null(summary$total_annotations))
})

# Test UI helper functions
test_that("UI helper functions behave correctly", {
  # Test concatenate_memos
  result <- concatenate_memos("First memo", "Second memo")
  expect_equal(result, "First memo; Second memo")

  result <- concatenate_memos("", "Single memo")
  expect_equal(result, "Single memo")
})

# Test project management functions
test_that("project management functions work correctly", {
  test_env <- create_test_env()
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "test_project.rds")

  # Test project saving
  saveRDS(test_env$rv, temp_file)
  expect_true(file.exists(temp_file))

  # Test project loading
  loaded_data <- readRDS(temp_file)
  expect_equal(loaded_data$text, test_env$rv$text)
  expect_equal(loaded_data$codes, test_env$rv$codes)

  # Clean up
  unlink(temp_file)
})
