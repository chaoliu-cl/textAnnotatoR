# Test add_theme function
test_that("add_theme adds themes correctly", {
  root <- Node$new("Root")

  # Test adding new theme
  root <- add_theme(root, "Theme1", "Description 1")
  expect_true(!is.null(root$children$Theme1))
  expect_equal(root$children$Theme1$description, "Description 1")
  expect_equal(root$children$Theme1$type, "theme")

  # Test duplicate theme error
  expect_error(add_theme(root, "Theme1"), "Theme already exists")
})

# Test add_code_to_theme function
test_that("add_code_to_theme works correctly", {
  root <- Node$new("Root")
  root <- add_theme(root, "Theme1")

  # Test adding code to root
  root <- add_code_to_theme(root, "Code1", character(0), "Description 1")
  expect_true(!is.null(root$children$Code1))
  expect_equal(root$children$Code1$type, "code")

  # Test adding code to theme
  root <- add_code_to_theme(root, "Code2", c("Theme1"), "Description 2")
  expect_true(!is.null(root$children$Theme1$children$Code2))

  # Test error for non-existent theme
  expect_error(
    add_code_to_theme(root, "Code3", c("NonExistentTheme")),
    "Theme not found: NonExistentTheme"
  )
})

# Test calculate_hierarchy_stats function
test_that("calculate_hierarchy_stats calculates correctly", {
  root <- Node$new("Root")
  root$type <- "theme"
  theme1 <- root$AddChild("Theme1")
  theme1$type <- "theme"
  code1 <- theme1$AddChild("Code1")
  code1$type <- "code"

  stats <- calculate_hierarchy_stats(root)

  expect_equal(stats$total_themes, 2)  # Root and Theme1
  expect_equal(stats$total_codes, 1)   # Code1
  expect_equal(stats$max_depth, 2)     # Root -> Theme1 -> Code1
  expect_type(stats$codes_per_theme, "list")
})
