# tests/testthat/test-analysis-functions.R

test_that("generate_code_frequency_plot handles inputs correctly", {
  # Test valid input
  valid_annotations <- data.frame(
    code = c("Code1", "Code2", "Code1", "Code3"),
    stringsAsFactors = FALSE
  )
  expect_no_error(generate_code_frequency_plot(valid_annotations))

  # Test empty annotations
  empty_annotations <- data.frame(
    code = character(0),
    stringsAsFactors = FALSE
  )
  expect_warning(generate_code_frequency_plot(empty_annotations))

  # Test invalid input
  expect_error(generate_code_frequency_plot("not_a_dataframe"))
  expect_error(generate_code_frequency_plot(data.frame(not_code = c(1,2,3))))
})

test_that("generate_code_co_occurrence_network handles edge cases", {
  # Test single code
  single_code <- data.frame(
    code = "Code1",
    start = 1,
    end = 5,
    stringsAsFactors = FALSE
  )
  expect_no_error(generate_code_co_occurrence_network(single_code))

  # Test empty annotations
  empty_annotations <- data.frame(
    code = character(0),
    start = numeric(0),
    end = numeric(0),
    stringsAsFactors = FALSE
  )
  expect_no_error(generate_code_co_occurrence_network(empty_annotations))
})

test_that("generate_text_summary calculates correctly", {
  text <- "This is the first paragraph.\n\nThis is the second paragraph."
  annotations <- data.frame(
    code = c("Code1", "Code2"),
    start = c(1, 30),
    end = c(25, 55),
    stringsAsFactors = FALSE
  )

  summary <- generate_text_summary(text, annotations)

  expect_equal(summary$total_words, 12)
  expect_equal(summary$total_paragraphs, 2)
  expect_equal(summary$total_annotations, 2)
  expect_equal(summary$unique_codes, 2)
})
