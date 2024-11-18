# tests/testthat/test-memo-management.R

test_that("concatenate_memos combines correctly", {
  expect_equal(concatenate_memos("", "First"), "First")
  expect_equal(concatenate_memos("First", "Second"), "First; Second")
  expect_equal(concatenate_memos("First; Second", "Third"), "First; Second; Third")
})

test_that("save_as_text creates valid file", {
  skip_if_not_installed("withr")

  withr::with_tempfile("temp", {
    expect_no_error(save_as_text(temp))
    expect_true(file.exists(temp))
  })
})

test_that("save_as_html creates valid file", {
  skip_if_not_installed("withr")

  withr::with_tempfile("temp", {
    expect_no_error(save_as_html(temp))
    expect_true(file.exists(temp))
    content <- readLines(temp)
    expect_true(any(grepl("<!DOCTYPE html>", content)))
  })
})
