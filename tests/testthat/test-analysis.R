# Test analyze_coverage function
test_that("analyze_coverage analyzes correctly", {
  annotations <- data.frame(
    start = c(1, 5, 10),
    end = c(3, 8, 15),
    code = c("code1", "code2", "code1"),
    stringsAsFactors = FALSE
  )

  results <- analyze_coverage(annotations)

  expect_type(results, "list")
  expect_true(!is.null(results$clusters))
  expect_true(!is.null(results$density))
  expect_true(!is.null(results$distribution))

  # Test empty annotations
  empty_results <- analyze_coverage(data.frame())
  expect_equal(empty_results$density$overall_density, 0)
})

# Test analyze_co_occurrences function
test_that("analyze_co_occurrences works correctly", {
  annotations <- data.frame(
    start = c(1, 2, 10),
    end = c(5, 6, 15),
    code = c("code1", "code2", "code3"),
    stringsAsFactors = FALSE
  )

  results <- analyze_co_occurrences(annotations)

  expect_type(results, "list")
  expect_true(!is.null(results$combinations))
  expect_true(!is.null(results$characteristics))

  # Test overlapping codes
  expect_true(results$characteristics$total_overlaps > 0)
})

# Test find_overlapping_codes function
test_that("find_overlapping_codes identifies overlaps correctly", {
  annotations <- data.frame(
    start = c(1, 3, 10),
    end = c(5, 7, 15),
    code = c("code1", "code2", "code3"),
    stringsAsFactors = FALSE
  )

  overlaps <- find_overlapping_codes(annotations)

  expect_type(overlaps, "list")
  expect_equal(length(overlaps), 1)  # One overlap between code1 and code2

  if (length(overlaps) > 0) {
    overlap <- overlaps[[1]]
    expect_equal(overlap$code1, "code1")
    expect_equal(overlap$code2, "code2")
    expect_equal(overlap$overlap_start, 3)
    expect_equal(overlap$overlap_end, 5)
  }
})

# Test generate_text_summary function
test_that("generate_text_summary produces correct statistics", {
  text <- "This is a test.\nIt has two paragraphs."
  annotations <- data.frame(
    start = c(1, 10),
    end = c(4, 15),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )

  summary <- generate_text_summary(text, annotations)

  expect_equal(summary$total_paragraphs, 2)
  expect_equal(summary$total_annotations, 2)
  expect_equal(summary$unique_codes, 2)
  expect_true(summary$total_characters > 0)
  expect_true(summary$total_words > 0)
})
