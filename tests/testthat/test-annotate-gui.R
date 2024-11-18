# tests/testthat/test-annotate-gui.R

library(testthat)
library(shiny)
library(jsonlite)
library(data.tree)

test_that("find_node_by_name works correctly", {
  # Create a test hierarchy
  root <- Node$new("Root")
  theme1 <- root$AddChild("Theme1")
  theme2 <- root$AddChild("Theme2")
  code1 <- theme1$AddChild("Code1")

  # Test finding existing nodes
  expect_equal(find_node_by_name(root, "Theme1")$name, "Theme1")
  expect_equal(find_node_by_name(root, "Code1")$name, "Code1")

  # Test non-existent node
  expect_null(find_node_by_name(root, "NonExistent"))

  # Test NULL inputs
  expect_null(find_node_by_name(NULL, "Theme1"))
  expect_null(find_node_by_name(root, NULL))
})

test_that("calculate_hierarchy_stats works correctly", {
  # Create a test hierarchy
  root <- Node$new("Root")
  root$type <- "theme"
  theme1 <- root$AddChild("Theme1")
  theme1$type <- "theme"
  code1 <- theme1$AddChild("Code1")
  code1$type <- "code"

  stats <- calculate_hierarchy_stats(root)

  expect_equal(stats$total_themes, 2)  # Root and Theme1
  expect_equal(stats$total_codes, 1)
  expect_equal(stats$max_depth, 2)
  expect_type(stats$codes_per_theme, "list")
})

test_that("add_code_to_theme works correctly", {
  root <- Node$new("Root")
  root$type <- "theme"

  # Add code to root
  root <- add_code_to_theme(root, "Code1", character(0), "Test code")
  expect_true(!is.null(root$children$Code1))
  expect_equal(root$children$Code1$type, "code")

  # Add theme and code to theme
  theme1 <- root$AddChild("Theme1")
  theme1$type <- "theme"
  root <- add_code_to_theme(root, "Code2", "Theme1", "Test code 2")

  expect_true(!is.null(root$children$Theme1$children$Code2))
  expect_equal(root$children$Theme1$children$Code2$type, "code")

  # Test invalid theme path
  expect_error(add_code_to_theme(root, "Code3", c("NonExistent"), "Test code 3"))
})

test_that("export_hierarchy and import_hierarchy work correctly", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()

  # Create a test hierarchy
  root <- Node$new("Root")
  root$type <- "theme"
  theme1 <- root$AddChild("Theme1")
  theme1$type <- "theme"
  code1 <- theme1$AddChild("Code1")
  code1$type <- "code"

  # Export hierarchy
  json_str <- export_hierarchy(root)
  temp_file <- file.path(temp_dir, "test_hierarchy.json")
  writeLines(json_str, temp_file)

  # Import hierarchy
  imported_json <- readLines(temp_file)
  imported_hierarchy <- import_hierarchy(paste(imported_json, collapse = "\n"))

  # Verify structure
  expect_equal(imported_hierarchy$name, "Root")
  expect_equal(imported_hierarchy$children$Theme1$name, "Theme1")
  expect_equal(imported_hierarchy$children$Theme1$children$Code1$name, "Code1")

  # Clean up
  unlink(temp_file)
})

test_that("analyze_code_patterns works correctly", {
  annotations <- data.frame(
    start = c(1, 20, 40),
    end = c(10, 30, 50),
    code = c("code1", "code2", "code1"),
    stringsAsFactors = FALSE
  )

  patterns <- analyze_code_patterns(annotations)

  expect_type(patterns, "list")
  expect_equal(patterns$summary$total_codes, 2)
  expect_equal(patterns$summary$unique_codes, 2)
})

test_that("analyze_co_occurrences works correctly", {
  annotations <- data.frame(
    start = c(1, 5, 20),
    end = c(10, 15, 30),
    code = c("code1", "code2", "code3"),
    stringsAsFactors = FALSE
  )

  results <- analyze_co_occurrences(annotations)

  expect_type(results, "list")
  expect_true(!is.null(results$combinations))
  expect_true(!is.null(results$characteristics))
})

test_that("compare_patterns works correctly", {
  # Create test coding strategies
  strategy1 <- list(
    coverage = list(density = list(overall_density = 0.5)),
    code_patterns = list(pattern1 = list(length = 10))
  )

  strategy2 <- list(
    coverage = list(density = list(overall_density = 0.7)),
    code_patterns = list(pattern1 = list(length = 15))
  )

  results <- compare_patterns(list(strategy1, strategy2))

  expect_type(results, "list")
  expect_true(!is.null(results$coverage_differences))
  expect_true(!is.null(results$code_differences))
})

test_that("generate_comparison_analysis works correctly", {
  # Create test annotation sets
  annotations1 <- data.frame(
    start = c(1, 20),
    end = c(10, 30),
    code = c("code1", "code2"),
    stringsAsFactors = FALSE
  )

  annotations2 <- data.frame(
    start = c(5, 25),
    end = c(15, 35),
    code = c("code1", "code3"),
    stringsAsFactors = FALSE
  )

  results <- generate_comparison_analysis(list(annotations1, annotations2))

  expect_type(results, "list")
  expect_true(!is.null(results$coding_strategies))
  expect_true(!is.null(results$comparison))
})
