# tests/testthat/test-project-state-management.R

test_that("save_project_state validates inputs", {
  # Test valid state
  valid_state <- list(
    text = "Sample text",
    annotations = data.frame(
      start = c(1, 10),
      end = c(5, 15),
      code = c("Code1", "Code2"),
      stringsAsFactors = FALSE
    ),
    codes = c("Code1", "Code2"),
    code_tree = Node$new("Root")
  )

  # Valid filename
  expect_no_error(
    withr::with_tempdir({
      save_project_state(valid_state, "test_project")
    })
  )

  # Invalid state (missing elements)
  invalid_state <- list(text = "Sample text")
  expect_error(
    save_project_state(invalid_state, "test_project"),
    "missing required elements"
  )

  # Invalid filename
  expect_error(
    save_project_state(valid_state, c("name1", "name2")),
    "filename must be a single character string"
  )
})

test_that("save_project_state creates project directory", {
  withr::with_tempdir({
    expect_false(dir.exists("projects"))

    valid_state <- list(
      text = "Sample text",
      annotations = data.frame(
        start = integer(0),
        end = integer(0),
        code = character(0),
        stringsAsFactors = FALSE
      ),
      codes = character(0),
      code_tree = Node$new("Root")
    )

    save_project_state(valid_state, "test_project")
    expect_true(dir.exists("projects"))
  })
})

test_that("load_project_state handles missing files", {
  withr::with_tempdir({
    # Test loading non-existent file
    expect_null(load_project_state("nonexistent.rds"))
  })
})

test_that("save and load project state preserves data", {
  withr::with_tempdir({
    # Create test data
    original_state <- list(
      text = "Sample text for testing",
      annotations = data.frame(
        start = c(1, 10),
        end = c(5, 15),
        code = c("Code1", "Code2"),
        memo = c("Memo1", "Memo2"),
        stringsAsFactors = FALSE
      ),
      codes = c("Code1", "Code2"),
      code_tree = Node$new("Root"),
      code_colors = c(Code1 = "#FF0000", Code2 = "#00FF00"),
      memos = list(
        list(type = "Code", link = "Code1", text = "Memo1"),
        list(type = "Document", link = "Document", text = "Memo2")
      )
    )

    # Save and load
    save_project_state(original_state, "test_project")
    loaded_state <- load_project_state("test_project.rds")

    # Compare components
    expect_equal(loaded_state$text, original_state$text)
    expect_equal(loaded_state$annotations, original_state$annotations)
    expect_equal(loaded_state$codes, original_state$codes)
    expect_equal(loaded_state$code_colors, original_state$code_colors)
    expect_equal(loaded_state$memos, original_state$memos)

    # Special handling for code_tree comparison since it's converted
    expect_s3_class(loaded_state$code_tree, "Node")
  })
})
