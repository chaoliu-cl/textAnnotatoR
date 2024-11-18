# tests/testthat/test-action-management.R

test_that("create_action validates inputs correctly", {
  # Test valid action creation
  expect_no_error(
    create_action(
      type = "add_annotation",
      data = list(start = 1, end = 10)
    )
  )

  # Test invalid type
  expect_error(
    create_action(
      type = "invalid_type",
      data = list()
    ),
    "type"
  )

  # Test invalid data
  expect_error(
    create_action(
      type = "add_annotation",
      data = "not_a_list"
    ),
    "data"
  )

  # Test invalid reverse_data
  expect_error(
    create_action(
      type = "add_annotation",
      data = list(),
      reverse_data = "not_a_list"
    ),
    "reverse_data"
  )
})

test_that("add_action handles action history correctly", {
  rv <- shiny::reactiveValues(
    action_history = list(),
    action_index = 0
  )

  action1 <- create_action(
    type = "add_annotation",
    data = list(start = 1, end = 10)
  )

  # Test adding first action
  expect_no_error(add_action(rv, action1))
  expect_equal(length(rv$action_history), 1)
  expect_equal(rv$action_index, 1)

  # Test adding second action
  action2 <- create_action(
    type = "add_annotation",
    data = list(start = 11, end = 20)
  )
  expect_no_error(add_action(rv, action2))
  expect_equal(length(rv$action_history), 2)
  expect_equal(rv$action_index, 2)

  # Test truncating future actions
  rv$action_index <- 1
  action3 <- create_action(
    type = "add_annotation",
    data = list(start = 21, end = 30)
  )
  expect_no_error(add_action(rv, action3))
  expect_equal(length(rv$action_history), 2)
  expect_equal(rv$action_index, 2)
})
