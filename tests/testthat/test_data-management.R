# test data management functions ---------------------
## function requires a data frame to be passed  ----
test_that("input is data frame", {

  input <- datasets::mtcars
  expect_true(is.data.frame(input))

  input <- "this is a string"
  expect_error(remove_right_empty_cols(input))

})

## function outputs a data frame with same rows as input ----
test_that("doesn't remove anything from df with data in rightmost column", {

  # dimensions should be the same
  input <-  datasets::mtcars
  output <- remove_right_empty_cols(input)

  expect_true(ncol(input) == ncol(output))
  expect_true(nrow(input) == nrow(output))

})

test_that("removes cols from df without data in rightmost column", {

  # cols should be different, rows should be same
  input <- data.frame(
    col1 = sample(1:100, 100, replace = T),
    col2 = NA,
    col3 = runif(100),
    col4 = sample(c("blue", "green", "yellow", "orange"), 100, replace = T),
    col5 = NA,
    col6 = NA
  )

  output <- remove_right_empty_cols(input)

  expect_false(ncol(input) == ncol(output))
  expect_true(nrow(input) == nrow(output))

})

## function only removes empty rows at the far right end ----
test_that("only removes right cols", {

  # set up test ----
  input <- data.frame(
    col0 = NA,
    col1 = sample(1:100, 100, replace = T),
    col2 = NA,
    col3 = runif(100),
    col4 = sample(c("blue", "green", "yellow", "orange"), 100, replace = T),
    col5 = NA,
    col6 = NA
  )

  output <- remove_right_empty_cols(input)

  expect_equal(c(colnames(output)) == c("col0", "col1", "col2", "col3", "col4"), rep(TRUE, 5))

})
