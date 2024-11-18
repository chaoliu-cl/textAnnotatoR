# tests/testthat/test-code-tree-management.R

test_that("get_code_names traverses tree correctly", {
  library(data.tree)

  # Create test tree
  root <- Node$new("Root")
  child1 <- root$AddChild("Code1")
  child2 <- root$AddChild("Code2")
  child1_1 <- child1$AddChild("Code1.1")

  codes <- get_code_names(root)

  expect_equal(length(codes), 4)
  expect_true(all(c("Root", "Code1", "Code2", "Code1.1") %in% codes))
})

test_that("get_code_paths generates correct paths", {
  library(data.tree)

  # Create test tree
  root <- Node$new("Root")
  child1 <- root$AddChild("Code1")
  child2 <- root$AddChild("Code2")
  child1_1 <- child1$AddChild("Code1.1")

  paths <- get_code_paths(root)

  expect_equal(length(paths), 3)  # Excluding root
  expect_true("Root/Code1" %in% paths)
  expect_true("Root/Code2" %in% paths)
  expect_true("Root/Code1/Code1.1" %in% paths)
})
