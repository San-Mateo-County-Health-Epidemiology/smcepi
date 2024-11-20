# test theme_gg_smc() ---------------------
## make sure it's a theme ----
test_that("output is a string", {
  path <- get_file_path(sort_method = "accessed date", sort_type = "newest")
  expect_true(is.character(path))
})

# expect_equal
