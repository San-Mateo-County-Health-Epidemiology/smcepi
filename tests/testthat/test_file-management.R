# test theme_gg_smc() ---------------------
## make sure it's a theme ----
test_that("output is a string", {

  path <- get_file_path(sort_method = "accessed date", sort_type = "newest")
  expect_true(is.character(path))

})

## check the input validation ----
test_that("input validation - sort_method", {

  expect_true(is.character(get_file_path(sort_method = "created date")))
  expect_true(is.character(get_file_path(sort_method = "accessed date")))
  expect_true(is.character(get_file_path(sort_method = "modified date")))
  expect_error(get_file_path(sort_method = "ctime"))

})

test_that("input validation - sort_type", {

  expect_true(is.character(get_file_path(sort_type = "newest")))
  expect_true(is.character(get_file_path(sort_type = "oldest")))
  expect_error(get_file_path(sort_type = "new"))

})

test_that("input validation - include_directories", {

  expect_true(is.character(get_file_path(include_directories = FALSE)))
  expect_true(is.character(get_file_path(include_directories = TRUE)))
  expect_error(get_file_path(include_directories = "yes"))

})

## big test - write files ----
test_that("sort behavior", {

  # set up test ----
  n_rows <- 1000
  data <- data.frame(
    replicate(10, sample(0:10000, n_rows, rep = TRUE)),
    replicate(10, runif(n_rows, 0.0, 1.0)),
    string = replicate(n_rows, paste(sample(c(letters, LETTERS), 10, rep = T), collapse = ""))
  )

  dir.create("file-testing")

  write.csv(data, file = "file-testing/data.csv")
  save(data, file = "file-testing/data1.Rdata")
  save(data, file = "file-testing/data2.Rdata")
  dir.create('file-testing/test_dir')

  # check newest file ----
  newest_file <- get_file_path(directory = "file-testing")
  expect_equal(newest_file, "file-testing/data2.Rdata")

  # check newest thing in folder ----
  newest_file <- get_file_path(directory = "file-testing", include_directories = T)
  expect_equal(newest_file, "file-testing/test_dir")

  # check oldest file ----
  newest_file <- get_file_path(directory = "file-testing", sort_type = "oldest")
  expect_equal(newest_file, "file-testing/data.csv")

  # check oldest .rdata ----
  newest_file <- get_file_path(directory = "file-testing", pattern = "*.Rdata$", sort_type = "oldest")
  expect_equal(newest_file, "file-testing/data1.Rdata")

  unlink("file-testing", recursive = T)

})
